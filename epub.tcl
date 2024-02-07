#!/usr/bin/tclsh

#
# EPUB_format_title removes apostrophes and replaces spaces with underlines.
#
proc EPUB_format_title {title} {
	return [string map { \' "" " " _ } $title]
}

#
# EPUB_paragraphs adds paragraph numbers to aid with editing. It returns the 
# number of paragraphs in the book.
#
proc EPUB_paragraphs {fn} {

	# Find the input file.
	if {![file exists $fn]} {
		puts "ERROR: Cannot find book file \"$fn\"."
		exit
	}
	set tfn [file join [file dirname $fn] "epub_temp.html"]
	
	# Open the document file for reading, and temporary file for writing.
	set f [open $fn r]
	set tf [open $tfn w]
	
	# Go through all lines and add counters.
	set count 0
	while {[gets $f line] >= 0} {
		if {[regexp {^<p>} $line]} {
			incr count
			set line [regsub {^<p>} $line "<p><b>$count\:</b> "]
		}
		puts $tf $line
	}
	
	# Close both files.
	close $f
	close $tf
	
	# Delete the original and replace with new file.
	file delete $fn
	file rename $tfn $fn

	# Return the number of paragraphs.
	return $count	
}

#
# EPUB_contents inserts a table of contents into an HTML document file, below an
# h3-level heading "Contents". Each other h2 heading will receive its own unique
# identifier, derived from the section title.
#
proc EPUB_contents {fn} {

	# Find the input file.
	if {![file exists $fn]} {
		puts "ERROR: Cannot find book file \"$fn\"."
		exit
	}
	set tfn [file join [file dirname $fn] "epub_temp.html"]
	
	# Open the document file for reading, and temporary file for writing.
	set f [open $fn r]
	set tf [open $tfn w]
	
	# Make a list of chapter titles and instert heading identifiers in text of
	# temporary file.
	set titles [list "Contents"]
	while {[gets $f line] >= 0} {
		if {[regexp {[^<]*<h2[^>]*>([^<]*)} $line match title]} {
			lappend titles $title
			set id [EPUB_format_title $title]
			puts $tf "<h2 id=\"$id\">$title</h2>"
		} else {
			puts $tf $line
		}
	}
	
	# Close both files.
	close $f
	close $tf
	
	# Open the document file for writing, and the temporary file for reading.
	set f [open $fn w]
	set tf [open $tfn r]
	
	# Copy the temporary file to the output file until and including h3-level
	# contents heading.
	while {[gets $tf line] > -1} {
		puts $f $line
		if {[regexp {[^<]*<h3[^>]*>[^<]*Contents} $line match]} {
			puts $f ""
			break
		}	
	}
	
	# Create table of contents after h3-level contents heading, but skipping the
	# first of our titles, which refers to the table of contents itself.
	foreach title [lrange $titles 1 end] {
		set id [EPUB_format_title $title]
		puts $f "<p class=\"toc\"><a href=\"#$id\">$title</a></p>"
	}
	
	# Skip one empty line, so that repeated applications of this process to the 
	# same document do not increase the number of blank lines between the table
	# of contents and the text.
	gets $tf line
	
	# Copy all lines except for table of content lines to the next file.
	while {[gets $tf line] >= 0} {
		if {![regexp {[^<]*<p class="toc">} $line match]} {
			puts $f $line
		}	
	}
	
	# Close the document and temporary files.
	close $f
	close $tf
	
	# Delete the temporary file.
	file delete $tfn
	
	# Return the list of chapter titles.
	return $titles
}	

#
# EPUB_partition splits an HTML document into chapters using h2-level headings
# as the chapter separators. The zero-chapter will be whatever is before the
# first h2-heading. The chapters are written as HTML files with names derived
# from the original file name, with a numerical index added, like BK_001.html.
# Every file shares the same header lines as were present at the beginning of
# the original file, before the first "body" tag. Chapter zero may contain a
# table of contents that refers to all the other chapters in order. We find all
# such internal file references of the original HTML document and add the new
# partitioned file name as prefix, so the partitioned table of contents will
# refer to the partitioned files.
#
proc EPUB_partition {fn} {

	# Find the input file.
	if {![file exists $fn]} {
		puts "ERROR: Cannot find book file \"$fn\"."
		exit
	}

	# Read input file.
	set f [open $fn r]
	set master [read $f]
	close $f
	
	# Extract the file header, which we will include above the body of every 
	# partition file. Remove the header from the 
	set n [string first "<body>" $master]
	if {$n < 0} {
		puts "ERROR: No <body> tag in \"$fn\"\."
		exit
	}
	set header [string range $master 0 [expr $n + 5]]
	set header [string trim $header]
	set master [string replace $master 0 [expr $n + 5] ""]
	
	# Partition the master according to h2-level html headings. The first partition 
	# will be the title page, table of contents, maps, and any other sections with
	# h3-level titles before we arrive at the first h2-level chapter title.
	set parts [list]
	while {1} {
		set n [string first "<h2" $master 3]
		if {$n < 0} {break}
		set part [string range $master 0 [expr $n - 1]]
		set part [string trim $part]
		lappend parts $part
		set master [string replace $master 0 [expr $n - 1] ""]
	}
	lappend parts $master
	
	# Prepare root name for the partition files.
	set fr [file rootname $fn]
	set root [file tail $fr]

	# Save the parts as root_$i.html
	set file_list [list]
	for {set i 0} {$i < [llength $parts]} {incr i} {
		set pfn $root\_[format %03d $i].html
		set f [open $pfn w]
		puts $f $header
		puts $f ""
		puts $f [lindex $parts $i]
		if {$i < [llength $parts] - 1} {
			puts $f "\n</body>\n</html>"
		}
		close $f
		lappend file_list $pfn
	}
	
	# In chapter zero, we look for class="toc" paragraphs and suffix the partition
	# file names in order. The first toc element is for the second file in our file
	# list, the first file being the table of contents file itself.
	set f [open [lindex $file_list 0] r]
	set tf [open temp_ch0.html w]
	set i 0
	while {[gets $f line] >= 0} {
		if {[regexp {class="toc"} $line]} {
			incr i
			set line [regsub {href="#} $line \
				"href=\"[file tail [lindex $file_list $i]]\#" ]
		}
		puts $tf $line
	}
	close $tf
	close $f
	file delete [lindex $file_list 0]
	file rename temp_ch0.html [lindex $file_list 0]

	# Return the list of files created.
	return $file_list
}

#
# EPUB_manifest opens an existing content.opf file and updates the file so that
# it declares every file in file_list, in both the manifest and spine elements.
# Any manifest elements up to and including the cover element will be retained.
# All those after the cover element will be removed.
#
proc EPUB_manifest {file_list} {

	# Find and open the content.opf file.
	set fn "content.opf"
	if {![file exists $fn]} {
		puts "ERROR: Cannot find the manifest and spine file \"$fn\"."
		exit
	}
	set f [open $fn r]
	
	# Open a temporary file.
	set tfn "temp_content.opf"
	set tf [open $tfn w]

	# Extract the contents of the file up to and including the manifest tag.
	while {[gets $f line] >= 0} {
		puts $tf $line
		if {[regexp {<manifest>} $line]} {break}
	}
	
	# Go through the existing elements of the manifest and add them to our
	# master string until we get past the ncx element, which should be the
	# final element we need to retain. The rest we will ignore, having added
	# our new entries.
	while {[gets $f line] >= 0} {
		if {[regexp {</manifest>} $line]} {
			puts "ERROR: No cover element in manifest of \"$fn\"."
			exit
		}
		puts $tf $line
		if {[regexp {id="ncx"} $line]} {break}
	}
	
	# Add elements for each file in our list.
	foreach part $file_list {
		set id [file root [file tail $part]]
		puts $tf "\t\t<item\
			id=\"$id\"\
			href=\"$part\"\
			media-type=\"application/xhtml+xml\"/>"
	}
	
	# End the manifest element.
	puts $tf "\t</manifest>\n"
	
	# Find the spine element.
	while {[gets $f line] >= 0} {
		if {[regexp {<spine} $line]} {
			puts $tf $line
			break
		}
	}
	
	# Write a spine element for each of the files in the list. Use the same ID
	# we used above. Compose a list of IDs, which we will return for use in
	# an epub table of contents.
	set id_list [list]
	foreach part $file_list {
		set id [file root [file tail $part]]
		lappend id_list $id
		puts $tf "\t\t<itemref idref=\"$id\"/>"
	}
	
	# Complete the file.
	close $f
	puts $tf "\t</spine>"
	puts $tf "</package>"
	close $tf
	
	# Delete the original file and rename the temporary file.
	file delete $fn
	file rename $tfn $fn
	
	# Return the list of identifiers.
	return $id_list
}

#
# EPUB_toc creates the toc.ncx file from the shell that exits on disk.
#
proc EPUB_toc {title_list file_list id_list} {

	# Find and open the toc.ncx file.
	set fn "toc.ncx"
	if {![file exists $fn]} {
		puts "ERROR: Cannot find the table of contents file \"$fn\"."
		exit
	}
	set f [open $fn r]
	
	# Open a temporary file.
	set tfn "temp_toc.ncx"
	set tf [open $tfn w]

	# Extract the contents of the file up to and including the navMap tag.
	while {[gets $f line] >= 0} {
		puts $tf $line
		if {[regexp {<navMap>} $line]} {break}
	}
	close $f
	
	# Add elements for each id, file, and title in our list.
	for {set i 0} {$i < [llength $file_list]} {incr i} {
		set part [lindex $file_list $i]
		set id [lindex $id_list $i]
		set title [lindex $title_list $i]
		puts $tf "\t\t<navPoint id=\"$id\" playOrder=\"[expr $i + 1]\">"
		puts $tf "\t\t\t<navLabel><text>$title</text></navLabel>"
		puts $tf "\t\t\t<content src=\"$part\"/>"
		puts $tf "\t\t</navPoint>"
	}
	
	# End the navMap element and the ncx element.
	puts $tf "\t</navMap>"
	puts $tf "</ncx>"
	close $tf	
	
	# Delete the original file and rename the temporary file.
	file delete $fn
	file rename $tfn $fn
	
	# Return empty string.
	return ""
}

#
# EPUB_metainf creates a META-INF directory and writes the required header
# to "container.xml" file within this directory. It creates a file called
# "mimetype" in the main directory, and writes the required content into 
# this file as well.
#
proc EPUB_metainf {} {
	set f [open mimetype w]
	puts -nonewline $f "application/epub+zip"
	close $f
	file mkdir META-INF
	set f [open META-INF/container.xml w]
	puts $f {<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
   <rootfiles>
      <rootfile full-path="content.opf" media-type="application/oebps-package+xml"/>
   </rootfiles>
</container>
}
	close $f
	return ""
}

#
# EPUB_cleanup deletes all the chapter files created by earlier activity,
# as well as the metadata files and directories created by the EPUB routines.
#
proc EPUB_cleanup {file_list} {
	foreach fn $file_list {file delete $fn}
	file delete "META-INF/container.xml"
	file delete "META-INF"
	file delete "mimetype"
}

puts "Epub Maker, Version 1.0"
puts "Copyright (C) 2024 by Kevan Hashemi"

set name [file tail [pwd]]
puts "Assuming book abbreviated name $name\."
set book "$name\.html"
puts "Will process file $book\."

puts "Generating internal table of contents."
set title_list [EPUB_contents $book]

puts "Partitioning book into chapters."
set file_list [EPUB_partition $book]

puts "Filling out manifest and spine."
set id_list [EPUB_manifest $file_list]

puts "Filling out table of contents."
EPUB_toc $title_list $file_list $id_list

puts "Generating meta files."
EPUB_metainf

set epub "../$name\.epub"
if {[file exists $epub]} {
	puts "Deleting existing ebook file."
	file delete $epub
}

puts "Compressing files to $epub."
exec zip -ruX $epub mimetype META-INF {*}[glob *] \
	--exclude epub.tcl \
	--exclude $book \
	--exclude readme.md
puts "Completed ebook is [expr [file size $epub]/1024] KB."

puts "Deleting all created files, leaving modified files."
EPUB_cleanup $file_list

puts "Generation complete, no errors."
