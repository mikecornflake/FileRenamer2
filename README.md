# FileRenamer2

This was/is intended to be a filenamer tool, offering:
+ Search / Replace
+ Common Case operations
+ Pascal Style string operations
+ Load a variety of metadata into a grid.  Use Column names in the string operations
+ User can create scripts

This is a rewrite of an existing hobby codebase - The first version was entirely complete, but I stupidly/lazily used third party components belonging to my employer (DevEx controls). 

Metatags supported by the first version:
+ EXIF
+ ID3
+ Windows Media Metadata
+ NTFS Storages

# Current status
Renaming and metadata saving is not implemented.  But this has been used as a framework for video processing.  I got ffprobe implemented to read video meta data, then was presented with a project requiring video processing based on existing metadata.  Most of the code had alrady been implemented in FileRenamer2, so I used this project for that...

This is my first multi-threaded project.  I keep hacking in additional processing in the main thread, then am slowly work back through them migrating them to worker threads...
