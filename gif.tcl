# Gif encoding in Pure Tcl.
# Copyright (C) 2004 Salvatore Sanfilippo - antirez at invece dot org -
# All Rights Reserved
#
# LICENSE
#
# Copyright (C) 2004 Salvatore Sanfilippo <antirez at invece dot org>
#
# The following terms apply to all files associated with the software
# unless explicitly disclaimed in individual files.
#
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
# 
# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 
#
# ------------------------------------------------------------------------------
#
# Note: if you want to understand this code, please make sure
# to read at the same time the GIF specification included in
# this tar.gz: gif89.txt.gz.

# TODO
# GifOptimize should ensure that transparency is preserved.

### Support functions ##########################################################

# Encode an unsigned 16 bit number in binary (big endian).
# [binary format] should have support for this.
proc GifEncode16 val {
    binary format cc [expr {$val&0xFF}] [expr {($val&0xFF00)>>8}]
}

# Returns the minimal number of bits needed to represent 'n'
proc GifBitsLen n {
    set res 0
    while {$n} {
	incr res
	set n [expr {$n/2}]
    }
    return $res
}

### Gif encoding ###############################################################

proc GifSubBlock data {
    set len [string length $data]
    if {$len > 255} {error "Sub-block is longer than 255 bytes"}
    return [binary format c $len]$data
}

proc GifBlockTerminator {} {
    GifSubBlock {}
}

proc GifHeader {} {
    return "GIF87a"
}

proc GifLogicalScreenDescriptor \
    {xres yres size {bgcolor 0} {res 5} {glob 1} {sort 0} {ratio 0}} \
{
    append data [GifEncode16 $xres][GifEncode16 $yres]
    set flags [expr {$size|$sort<<3|$res<<4|$glob<<7}]
    append data [binary format ccc $flags $bgcolor $ratio]
}

proc GifGlobalColorTable colors {
    set colortable {}
    foreach {r g b} $colors {
	append colortable [binary format ccc $r $g $b]
    }
    return $colortable
}

proc GifImageDescriptor {xres yres} {
    append data "\x2c" ;# Image separator byte
    append data "\x00\x00\x00\x00" ;# Left/Top position in the virtual screen
    append data [GifEncode16 $xres][GifEncode16 $yres]
    append data "\x00" ;# flags: no local colortab, no interlace.
}

proc GifLzwCodeSize {codesize} {
    binary format c $codesize
}

proc GifTrailer {} {
    return "\x3b"
}

# Pad the color table to the next power of two length.
proc GifPaddedColorTable colortab {
    set bits [GifBitsLen [expr {([llength $colortab]/3)-1}]]
    set newlen [expr {1<<$bits}]
    if {$newlen == 1} {set newlen 2}
    while {$newlen > ([llength $colortab]/3)} {
	lappend colortab 255 255 255
    }
    return $colortab
}

# This function gets the width, height, colortab, and pixels list
# of an image and returns a full-formed GIF file ready to be transfered
# into a file.
#
# The colortab is just a list of R G B components, for example
# a black & white colortab is: [list 0 0 0 255 255 255].
#
# Pixels is a list of indexes in the color table, the list must
# be width*height in length.
proc GifEncode {xres yres colortab pixels} {
    set bpp [GifBitsLen [expr {([llength $colortab]/3)-1}]]
    if {$bpp == 0} {set bpp 1}
    if {$bpp < 1 || $bpp > 8} {
	error "The number of different colors must be in the range 2-256"
    }
    set colortab [GifPaddedColorTable $colortab]
    append gif [GifHeader]
    append gif [GifLogicalScreenDescriptor $xres $yres [expr {$bpp-1}]]
    append gif [GifGlobalColorTable $colortab]
    append gif [GifImageDescriptor $xres $yres]
    append gif [GifLzwEncode $bpp $pixels]
    append gif [GifTrailer]
}

# Optimize an image before the encoding process.
# That's an optional step, but may allow the LZW compression to
# do a better work.
proc GifOptimize {colortabVar pixelsVar} {
    upvar 1 $colortabVar colortab $pixelsVar pixels

    # Check if the optimization is needed
    set tabcolors [expr {[llength $colortab]/3}]
    array set index {}
    foreach pixel $pixels {
	set index($pixel) {}
    }
    set usedcolors [array size index]
    if {$usedcolors >= $tabcolors} return
    # Assign a new contiguos index to every old index
    # and creates a new colormap.
    set newidx 0
    foreach oldidx [lsort -integer [array names index]] {
	lappend newcolortab [lindex $colortab [expr {$oldidx*3}]]
	lappend newcolortab [lindex $colortab [expr {$oldidx*3+1}]]
	lappend newcolortab [lindex $colortab [expr {$oldidx*3+2}]]
	set old2new($oldidx) $newidx
	incr newidx
    }
    # Convert every pixel according to the new indexes.
    for {set i 0} {$i < [llength $pixels]} {incr i} {
	lset pixels $i $old2new([lindex $pixels $i])
    }
    set colortab $newcolortab
    return {}
}

### GIF Bitstream Handling  ####################################################

# Format used:
# In GIF images the codes are formed into a stream of bits as if they were
# packed right to left and then picked off 8 bits at a time to be output.
# Assuming to work with 5 bits indexes that's a representation of the
# encoding of 5-bits codes in 8-bits bytes:
#
# bbbaaaaa
# dcccccbb
# eeeedddd
# ...
#
# and so on.

# Returns a new bitstream value, that's just a three-elements list containing
# a string (the bytes array), a number representing the last byte of
# the stream not yet "full" of bits, and an integer representing the number of
# unused bits in this last byte.
#
# Bitsreams support an "append" operation where integers of a specified
# bit are continuously appended. This is used to support LZW encoding.
proc GifBitstream {} {
    # start with an empty stream, a zero partial byte, and 8 left bits
    # in this partial byte.
    list {} 0 8
}

# Append a value of 'bits' bits to the bitstreamVar variable.
proc GifBitstreamAppend {bitstreamVar bits value} {
    upvar 1 $bitstreamVar bs
    set stream [lindex $bs 0]
    set lastbyte [lindex $bs 1]
    set leftbits [lindex $bs 2]
    set bs {} ; # To kill the old list object is an hack to gain performances.
    while {$bits} {
	# Split the value in two parts: the part we can
	# encode in the current byte, and the part that will be
	# encoded in the next iterations.
	if {$bits > $leftbits} {
	    set newbits [expr {$bits-$leftbits}]
	    set newvalue [expr {$value>>$leftbits}]
	    set value [expr {$value&((1<<$leftbits)-1)}]
	    set bits $leftbits
	} else {
	    set newbits 0
	    set newvalue $value
	}
	# Encode the bits
	set value [expr {$value<<(8-$leftbits)}]
	set lastbyte [expr {$value|$lastbyte}]
	set leftbits [expr {$leftbits-$bits}]
	# Append this byte on the stream and create a new 'last byte' if needed.
	if {$leftbits == 0} {
	    append stream [binary format c $lastbyte]
	    set lastbyte 0
	    set leftbits 8
	}
	# Prepare the next iteration with the left bits if any.
	set bits $newbits
	set value $newvalue
    }
    set bs [list $stream $lastbyte $leftbits]
    return {}
}

# Returns the stream as an unique string, right-padded with zeros
# if the number of bits contained is not multiple of 8.
proc GifBitstreamString bitstream {
    set str [lindex $bitstream 0]
    if {[lindex $bitstream 2] != 8} {
	append str [binary format c [lindex $bitstream 1]]
    }
    return $str
}

### LZW compression ############################################################

# Lzw compression. The algorithm uses by GIF variable code length
# encoding and two special codes, "Reset Code Table" and "End Of Data".
# The code length supported by GIF can be from 2 to 12 bits, starting
# from a code length of max(bpp,2)+1. Once the max code for the current
# code length is used, the algorithm starts usign one bit more for the
# codelength. If the code (2^12)-1 is reached, the algorithm outputs
# a "Reset Code Table" and restart using 12 bits.
proc GifLzwEncode {bpp pixels} {
    if {$bpp == 1} {incr bpp}
    set stream [GifBitstream]
    set codesize [expr {$bpp+1}]
    set clearcode [expr {1<<$bpp}]
    set endcode [expr {$clearcode+1}]
    # Start of the LZW encoding
    set prefix {}
    for {set i 0} {$i < $clearcode} {incr i} {
	set table([binary format c $i]) $i
    }
    set nextcode [expr {$endcode+1}]
    # Output a leading clear-code.
    GifBitstreamAppend stream $codesize $clearcode
    foreach pixel $pixels {
	set byte [binary format c $pixel]
	if {[info exists table($prefix$byte)]} {
	    append prefix $byte
	} else {
	    set outcode $table($prefix)
	    GifBitstreamAppend stream $codesize $outcode
	    set table($prefix$byte) $nextcode
	    incr nextcode
	    set prefix $byte
	    if {($nextcode-1) == [expr {(1<<$codesize)}]} {
		if {$codesize == 12} {
		    GifBitstreamAppend stream $codesize $clearcode
		    set codesize [expr {$bpp+1}]
		    set clearcode [expr {1<<$bpp}]
		    set endcode [expr {$clearcode+1}]
		    unset table
		    for {set i 0} {$i < $clearcode} {incr i} {
			set table([binary format c $i]) $i
		    }
		    set nextcode [expr {$endcode+1}]
		    # Note that we need to set as prefix the current
		    # byte, we now it's in the new (initial) table.
		    set prefix $byte
		} else {
		    incr codesize
		}
	    }
	}
    }
    if {$prefix ne {}} {
	    GifBitstreamAppend stream $codesize $table($prefix)
    }
    # Output the final end-code
    GifBitstreamAppend stream $codesize $endcode
    # Split in 255 bytes blocks
    set blocks {}
    set lzwoutput [GifBitstreamString $stream]
    while {[string length $lzwoutput]} {
	lappend blocks [string range $lzwoutput 0 254]
	set lzwoutput [string range $lzwoutput 255 end]
    }
    # Append the codesize, blocks, and terminator, and return the whole stuff.
    append gif [GifLzwCodeSize $bpp]
    foreach block $blocks {
	append gif [GifSubBlock $block]
    }
    append gif [GifBlockTerminator]
}

### Testing ####################################################################

proc GifTest {} {
    for {set i 0} {$i < 256} {incr i} {
	lappend colortab $i $i $i
    }
    for {set i 0} {$i < (256*256)} {incr i} {
	lappend pixels [expr {int($i)%256}]
    }
    set gif [GifEncode 256 256 $colortab $pixels]
    set fd [open my.gif w]
    fconfigure $fd -translation binary
    puts -nonewline $fd $gif
    close $fd
}

GifTest
