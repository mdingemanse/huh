dirF$ = "/Volumes/fgkastuf/PUB/HSSLU/Other Initiated Repair/Interjections/Coding/Francisco/coded"
dirN$ = "/Volumes/fgkastuf/PUB/HSSLU/Other Initiated Repair/Interjections/Coding/Nick"
dirM$ = "/Volumes/fgkastuf/PUB/HSSLU/Other Initiated Repair/Interjections/Coding/Mark/Mark"

Create Strings as file list... fileList 'dirF$'/*.TextGrid
ns = Get number of strings

Create Table with column names... table ns name iF mF oF nF vF iM mM oM nM vM iN mN oN nN vN iA mA oA nA vA iFM mFM oFM nFM vFM iMN mMN oMN nMN vMN iFN mFN oFN nFN vFN notesF notesM notesN language

#r-o-z-n-a

for i to ns
	select Strings fileList
	gridName$ = Get string... i
	name$ = gridName$ - ".TextGrid"
	
	Read from file... 'dirF$'/'gridName$'
	label$ = Get label of interval... 1 1
	notesF$ = Get label of interval... 2 1
	iF$ = mid$(label$, 1, 1)
	mF$ = mid$(label$, 3, 1)
	oF$ = mid$(label$, 5, 1)
	nF$ = mid$(label$, 7, 1)
	vF$ = mid$(label$, 9, 1)
	if mF$ = "c" and vF$ = "@"
		vF$ = "x"
	endif

	if notesF$ = ""
		notesF$ = "NA"
	endif


	Read from file... 'dirM$'/'gridName$'
	label$ = Get label of interval... 1 1
	notesM$ = Get label of interval... 2 1
	iM$ = mid$(label$, 1, 1)
	mM$ = mid$(label$, 3, 1)
	oM$ = mid$(label$, 5, 1)
	nM$ = mid$(label$, 7, 1)
	vM$ = mid$(label$, 9, 1)
	if mM$ = "c" and vM$ = "@"
		vM$ = "x"
	endif

	if notesM$ = ""
		notesM$ = "NA"
	endif

	Read from file... 'dirN$'/'gridName$'
	label$ = Get label of interval... 1 1
	notesN$ = Get label of interval... 2 1
	iN$ = mid$(label$, 1, 1)
	mN$ = mid$(label$, 3, 1)
	oN$ = mid$(label$, 5, 1)
	nN$ = mid$(label$, 7, 1)
	vN$ = mid$(label$, 9, 1)
	if mN$ = "c" and vN$ = "@"
		vN$ = "x"
	endif

	if notesN$ = ""
		notesN$ = "NA"
	endif

	iA$ = "False"
	mA$ = "False"
	oA$ = "False"
	nA$ = "False"
	vA$ = "False"

	if (iF$ = iM$) and (iM$ = iN$)
		iA$ = "True"
	endif

	if (mF$ = mM$) and (mM$ = mN$)
		mA$ = "True"
	endif

	if (oF$ = oM$) and (oM$ = oN$)
		oA$ = "True"
	endif

	if (nF$ = nM$) and (nM$ = nN$)
		nA$ = "True"
	endif

	if (vF$ = vM$) and (vM$ = vN$)
		vA$ = "True"
	endif


	## Francisco and Mark
	iFM$ = "False"
	mFM$ = "False"
	oFM$ = "False"
	nFM$ = "False"
	vFM$ = "False"

	if (iF$ = iM$)
		iFM$ = "True"
	endif

	if (mF$ = mM$)
		mFM$ = "True"
	endif

	if (oF$ = oM$)
		oFM$ = "True"
	endif

	if (nF$ = nM$)
		nFM$ = "True"
	endif

	if (vF$ = vM$)
		vFM$ = "True"
	endif

	
	## Francisco and Nick
	iFN$ = "False"
	mFN$ = "False"
	oFN$ = "False"
	nFN$ = "False"
	vFN$ = "False"

	if (iF$ = iN$)
		iFN$ = "True"
	endif

	if (mF$ = mN$)
		mFN$ = "True"
	endif

	if (oF$ = oN$)
		oFN$ = "True"
	endif

	if (nF$ = nN$)
		nFN$ = "True"
	endif

	if (vF$ = vN$)
		vFN$ = "True"
	endif

	## Mark and Nick
	iMN$ = "False"
	mMN$ = "False"
	oMN$ = "False"
	nMN$ = "False"
	vMN$ = "False"

	if (iM$ = iN$)
		iMN$ = "True"
	endif

	if (mM$ = mN$)
		mMN$ = "True"
	endif

	if (oM$ = oN$)
		oMN$ = "True"
	endif

	if (nM$ = nN$)
		nMN$ = "True"
	endif

	if (vM$ = vN$)
		vMN$ = "True"
	endif

	language$ = "NA"
	where = 0
	
	where = index(name$, "Chapalaa")
	if where > 0
		language$ = "Chapalaa"
	endif

	where = index(name$, "Duna")
	if where > 0
		language$ = "Duna"
	endif

	where = index(name$, "Dutch")
	if where > 0
		language$ = "Dutch"
	endif

	where = index(name$, "Icelandic")
	if where > 0
		language$ = "Icelandic"
	endif

	where = index(name$, "Italian")
	if where > 0
		language$ = "Italian"
	endif

	where = index(name$, "Lao")
	if where > 0
		language$ = "Lao"
	endif

	where = index(name$, "Mandarin")
	if where > 0
		language$ = "Madarin"
	endif

	where = index(name$, "MP")
	if where > 0
		language$ = "MP"
	endif

	where = index(name$, "Russian")
	if where > 0
		language$ = "Russian"
	endif

	where = index(name$, "Siwu")
	if where > 0
		language$ = "Siwu"
	endif

	where = index(name$, "Sp")
	if where > 0
		language$ = "Spanish"
	endif

	select Table table
	Set string value... i name 'name$'
	Set string value... i iF 'iF$'
	Set string value... i mF 'mF$'
	Set string value... i oF 'oF$'
	Set string value... i nF 'nF$'
	Set string value... i vF 'vF$'
	Set string value... i notesF 'notesF$'
	Set string value... i iM 'iM$'
	Set string value... i mM 'mM$'
	Set string value... i oM 'oM$'
	Set string value... i nM 'nM$'
	Set string value... i vM 'vM$'
	Set string value... i notesM 'notesM$'
	Set string value... i iN 'iN$'
	Set string value... i mN 'mN$'
	Set string value... i oN 'oN$'
	Set string value... i nN 'nN$'
	Set string value... i vN 'vN$'
	Set string value... i notesN 'notesN$'

	Set string value... i iA 'iA$'
	Set string value... i mA 'mA$'
	Set string value... i oA 'oA$'
	Set string value... i nA 'nA$'
	Set string value... i vA 'vA$'

	Set string value... i iFM 'iFM$'
	Set string value... i mFM 'mFM$'
	Set string value... i oFM 'oFM$'
	Set string value... i nFM 'nFM$'
	Set string value... i vFM 'vFM$'

	Set string value... i iFN 'iFN$'
	Set string value... i mFN 'mFN$'
	Set string value... i oFN 'oFN$'
	Set string value... i nFN 'nFN$'
	Set string value... i vFN 'vFN$'

	Set string value... i iMN 'iMN$'
	Set string value... i mMN 'mMN$'
	Set string value... i oMN 'oMN$'
	Set string value... i nMN 'nMN$'
	Set string value... i vMN 'vMN$'

	Set string value... i language 'language$'

	select all
	minus Table table
	minus Strings fileList
	Remove

endfor