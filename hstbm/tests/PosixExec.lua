#!/usr/bin/env lua

--- PosixExec -- Run commands with fine control over input/output streams
-- Returns a table with the functions:
--     raw_exec(Command, CmdlineArgs)     -- Returns pid, fd0, fd1, fd2
--     exec(Command, CmdlineArgs)         -- Writes output to stdout; also
--                                           returns output as a string
--     exec_quietly(Command, CmdlineArgs) -- Returns output as a string
--     raw_close(fd0, fd1, fd2)
-- These functions return (nil, ReasonString) if the request fails.

--[[
NOTE: Nearly all files in hstbm are derived from GNU Grep, and so are
owned by the FSF and have GPL3 license; however, a few files, heavily
dependent on Lua and created independently of GNU Grep, are owned by
Grouse Software, and have the MIT License in order to be compatible
with the Lua ecosystem.  Any file not explicitly marked is intended
to be FSF-owned and GPL3-licensed.

This file (PosixExec.lua) is licensed under the MIT License.
--]]

--[[
Copyright (C) 2014-2015 Grouse Software
Written by behoffski (Brenton Hoff).

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--]]

local io     = require("io")
local os     = require("os")
local string = require("string")

-- luarocks modules
require("luarocks.loader")
local posix  = require("posix")
local strict = require("strictness")

local M = {}

-- Conservative (POSIX.1?) SSIZE_MAX for individual read() operations.
local POSIX_SSIZE_MAX = 32767

------------------------------------------------------------------------------

-- Return a pipe, where read and write FDs are both guaranteed to be > 2.
-- This is needed as values 0/1/2 are special: stdout/stdin/stderr

local function get_nonstd_pipe()
        local rd = {}
        local wr = {}
        local pipe = 1

        -- Keep acquiring pipes until a suitable pair is found, or we
        -- get results that we believe are highly suspect.
        while true do
                rd[pipe], wr[pipe] = assert(posix.pipe())
                if rd[pipe] > 2 and wr[pipe] > 2 then
                        break
                end

                -- We should need no more than 4 loops to find a suitable
                -- pair of FDs (assuming previous FDs are not reused).
                -- Here, we let the loop run up to 5 times, and if we
                -- haven't found a suitable pair after this effort, our
                -- assumptions are wrong, and/or perhaps the system state
                -- is suspect; in any case, abort all work.
                pipe = pipe + 1
                if pipe > 5 then
                        pipe = false
                        break
                end
        end

        -- Raise an error if we couldn't obtain a suitable pipe
        if not pipe then
                error("Cannot obtain pipe with FDs both > 2")
        end

        -- Discard any unsuitable pipes
        for i = 1, pipe - 1 do
                assert(posix.close(rd[i]))
                assert(posix.close(wr[i]))
        end

        -- Return pipe FDs to caller
        return rd[pipe], wr[pipe]
end

------------------------------------------------------------------------------

function M.raw_exec(Command, CmdlineArgs)
        assert(Command, "missing command name")
--[[
        io.write("Command + Args: ", Command)
	for _, v in ipairs(CmdlineArgs) do
		io.write(" ", v)
	end
        io.write("\n")
--]]
        local rd0, wr0 = get_nonstd_pipe()
        local rd1, wr1 = get_nonstd_pipe()
        local rd2, wr2 = get_nonstd_pipe()
        local pid = posix.fork()
        if pid < 0 then
                return nil, "raw_exec: Unable to fork(): " .. posix.errno(pid)
        end
        if pid ~= 0 then
                -- Parent: Close child end of pipes
                assert(posix.close(rd0))
                assert(posix.close(wr1))
                assert(posix.close(wr2))

                -- Report PID and piped FDs to caller
                return pid, wr0, rd1, rd2
        end

        -- Child: Close parent end of pipes
        assert(posix.close(wr0))
        assert(posix.close(rd1))
        assert(posix.close(rd2))

        -- Change child's stdout/stdin/stderr to use its end of the pipes
        assert(posix.dup2(rd0, posix.fileno(io.stdin)))
        assert(posix.dup2(wr1, posix.fileno(io.stdout)))
        assert(posix.dup2(wr2, posix.fileno(io.stderr)))

        -- Finally, execute the requested command in the child environment
        assert(posix.exec(Command, CmdlineArgs))

        -- We should never get here
        error("Child: posix.execp returned unexpectedly")
end

------------------------------------------------------------------------------

function M.ScanPollFDs(ThisExec)
        local OutputReceived = false
        for fd in pairs(ThisExec.PollFDs) do
                if ThisExec.PollFDs[fd].revents.IN then
                        local Buf, Return =
                                assert(posix.read(fd, POSIX_SSIZE_MAX))
                        if Return == nil then
                                Return = "(nil)"
                        end
                        if Buf then
                                OutputReceived = true
                                local FDOut = ThisExec.FDs[fd].Output
                                FDOut[#FDOut + 1] = Buf
                                if not ThisExec.Quietly then
                                        assert(io.write(Buf))
                                        assert(io.flush())
                                end
                        elseif Return ~= nil then
                                print("Error message: " .. Return)
                                break
                        end

                elseif ThisExec.PollFDs[fd].revents.HUP then
--                        posix.close(fd)
                        ThisExec.PollFDs[fd] = nil
                        if not next(ThisExec.PollFDs) then
                                ThisExec.ChildRunning = false
                                break
                        end
                end
        end
end

------------------------------------------------------------------------------

function M.backend_exec(Quietly, Command, CmdlineArgs)
        local pid, fd0, fd1, fd2 = M.raw_exec(Command, CmdlineArgs)
        if not pid then
                error(fd0)
        end

        local ThisExec = {
                Quietly = Quietly,
                ChildRunning = true,
                FDs = {},
                PollFDs = {},
                PollTimeout = 60000,
        }

        -- Monitor stdout (fd1) and stderr (fd2)
        ThisExec.PollFDs[fd1] = { events = {IN = true}, }
        ThisExec.PollFDs[fd2] = { events = {IN = true}, }
        ThisExec.FDs[fd1] = {Output = {}, }
        ThisExec.FDs[fd2] = {Output = {}, }
        while ThisExec.ChildRunning do
                local Ret, Err = posix.poll(ThisExec.PollFDs,
                                            ThisExec.PollTimeout)
                if Err == nil then
                        Err = "(nil)"
                end
--[[
                print(string.format("poll Ret, Err: %q, %q", Ret, Err))
--]]
                if Ret > 0 then
                        -- Handle activity named in poll revents table
                        M.ScanPollFDs(ThisExec)
                end
        end

        -- Child exited (HUP), clean up zombie process and get exit status
        local WaitPid, DemiseErrStr, DemiseErrNum = posix.wait(pid)

--        local ErrStr, errno = posix.errno()
        M.raw_close(fd0, fd1, fd2)
--[[
        if WaitPid < 0 then
                return nil, ErrStr, errno
        end
--]]
        local Stdout = table.concat(ThisExec.FDs[fd1].Output)
        local Stderr = table.concat(ThisExec.FDs[fd2].Output)
        return Stdout, Stderr, DemiseErrNum, DemiseErrStr
end

------------------------------------------------------------------------------

function M.exec_quietly(Command, CmdlineArgs)
        return M.backend_exec(true, Command, CmdlineArgs)
end


------------------------------------------------------------------------------

function M.exec(Command, CmdlineArgs)
        return M.backend_exec(false, Command, CmdlineArgs)
end

------------------------------------------------------------------------------

function M.raw_close(fd0, fd1, fd2)
        assert(posix.close(fd0))
        assert(posix.close(fd1))
        if fd2 then
                assert(posix.close(fd2))
        end
end

-----------------------------------------------------------------------------

function M.PretendToExec(Command, ...)
        local Sanitised = {"# " .. Command}
        for _, v in ipairs{...} do
                local Quoted = string.format("%q", v)
                if Quoted == '"' .. v .. '"' then
                        Sanitised[#Sanitised + 1] = v
                else
                        Sanitised[#Sanitised + 1] = Quoted
                end
        end
        print(table.concat(Sanitised, " "))
end

-----------------------------------------------------------------------------

function M.PretendToChdir(Path)
        local Quoted = string.format("%q", Path)
        if Quoted == '"' .. Path .. '"' then
                print("# chdir " .. Path)
        else
                print("# chdir " .. Quoted)
        end
end

------------------------------------------------------------------------------

-- Report public functions to caller
return M

