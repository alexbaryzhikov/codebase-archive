dirlist, err = mydir( path )
if err then
	print( "Error reading directory: "..err )
else
	for i, v in ipairs( dirlist ) do
	 	print( v )
	end
end
