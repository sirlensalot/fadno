#!/usr/bin/osascript

on run(arguments)
   set fpath to POSIX path of (first item of arguments)
   set AppleScript's text item delimiters to "/"
   set fname to text item -1 of fpath
   --display dialog fname
   tell application "QuickTime Player 7"
     try
		repeat with i from 1 to the 30
			tell document i
				set dname to get name
				if dname is fname then
				   close
				end if
			end tell
		end repeat
    on error
	end try
	open fpath
	delay 0.5
	tell document 1
	  play
	end tell
  end tell
end run
