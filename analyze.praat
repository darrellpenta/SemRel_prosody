

form Save acoustics to file
	comment What directory do you want to load?

	sentence Directory ./Subject01/

        comment Where do you want to save the results?

	text textfile acoustics.txt
endform

Create Strings as file list... list 'directory$'*
numberOfFiles = Get number of strings

for ifile to numberOfFiles

	select Strings list

	filename$ = Get string... ifile

	if right$ (filename$, 5) = ".aiff"

		soundid$ = left$(filename$, (length (filename$) - 5))

		echo 'directory$''filename$'
	
		Open long sound file... 'directory$''filename$'
		Read from file... 'directory$''soundid$'.TextGrid
	
		select TextGrid 'soundid$'
		number_of_intervals = Get number of intervals... 1
		number_of_intervals = number_of_intervals
		
		for interval from 1 to number_of_intervals
		
			select TextGrid 'soundid$'
			text$ = Get label of interval... 1 'interval'
		
			if text$ <> ""
	
				utterance_start = Get starting point... 1 interval
				utterance_end = Get end point... 1 interval
				utterance_duration = utterance_end - utterance_start
			
				select LongSound 'soundid$'
				Extract part... utterance_start utterance_end yes
				#now Sound soundname$ is on the stack 
				select Sound 'soundid$'
				To Pitch (ac)... 0 75 15 no 0.03 0.45 0.01 0.35 0.14 600.0
				select Pitch 'soundid$'
			
				
				duration = Get total duration
				min = Get minimum... 0 0 Hertz Parabolic
				max = Get maximum... 0 0 Hertz Parabolic
				mean = Get mean... 0 0 Hertz
				slope = Get mean absolute slope... Hertz
			
				# append the label and the duration to the end of the text file, separated with a tab:		
			
				resultline$ = "'" + "'soundid$'" + "'" + tab$ + "'" + "'text$'" + "'" + tab$ + "'duration'" + tab$ + "'min'" +tab$+ "'max'" +tab$+ "'mean'" +tab$+ "'slope'" + newline$
			
				fileappend "'textfile$'" 'resultline$'
			
				# and remove our sound
				select Sound 'soundid$'
				Remove
				select Pitch 'soundid$'
				Remove

			endif # if not empty

		endfor #end loop over intervals
	
		# remove these things
		select LongSound 'soundid$'
		Remove
		select TextGrid 'soundid$'
		Remove
	endif # end if right file type
endfor # end loop over files


select Strings list
Remove












