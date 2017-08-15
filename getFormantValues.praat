dir$ = "/Users/frator/Documents/Repairs/Formant_data"

Create Strings as file list... fileList 'dir$'/*.TextGrid
ns = Get number of strings

clearinfo
printline name	f1	f2	f3	language

for i to ns
	select Strings fileList
	gridName$ = Get string... i
	name$ = gridName$ - ".TextGrid"
	language$ = left$(name$, 2)
	Read from file... 'dir$'/'name$'.wav
	Read from file... 'dir$'/'name$'.TextGrid
	label$ = Get label of interval... 1 2
	start = Get start point... 1 2
	end = Get end point... 1 2
	select Sound 'name$'
	Extract part... start end rectangular 1 no
	To Intensity... 100 0 yes
	where = Get time of maximum... 0 0 Parabolic
	Remove

	select Sound 'name$'_part
	To Formant (burg)... 0 5 5500 0.025 50
	f1 = Get value at time... 1 where Hertz Linear
	f2 = Get value at time... 2 where Hertz Linear
	f3 = Get value at time... 2 where Hertz Linear
	if label$ = "" & f1 < 1000
		printline 'name$'	'f1:0'	'f2:0'	'f3:0'	'language$'
	endif
	select all
	minus Strings fileList
	Remove
endfor

select all
Remove