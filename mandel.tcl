# Mandelbrot example for the GIF package.
# Copyright (C) 2004 Salvatore Sanfilippo
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


source gif.tcl

set tcl_precision 17

proc perc {part tot} {
    format "%.2f" [expr {100*$part/$tot}]
}

proc lineputs s {
    set len [string length $s]
    set del [string repeat "\b" $len]
    set space [string repeat " " $len]
    puts -nonewline $del$space$del$s
    flush stdout
}

proc mandel {xres yres {infx -2.0} {infy -1.5} {supx 1.0} {supy 1.5}} {
    set latox [expr {$supx-$infx}]
    set latoy [expr {$supy-$infy}]
    set incremx [expr {double($latox)/($xres+1)}]
    set incremy [expr {double($latoy)/($yres+1)}]
    for {set i 0} {$i < $yres} {incr i} {
	    set cre [expr {$infx+($i*$incremx)}]
	    for {set j 0} {$j < $xres} {incr j} {
		    set counter 0
		    set zim 0
		    set zre 0
		    set cim [expr {$infy+($j*$incremy)}]
		    while {($zre*$zre+$zim*$zim <= 4) && $counter <= 126} {
			    set dam [expr {$zre*$zre-$zim*$zim+$cre}]
			    set zim [expr {2*$zim*$zre+$cim}]
			    set zre $dam
			    incr counter
		    }
		    set col [expr {$counter * 2}]
		    lappend pixels $col
	    }
	    lineputs "[perc [expr {$i+1}] $yres]% done"
    }
    puts {}
    return $pixels
}

proc pi {} {return 3.1415926535897931}

proc max args {
    set m [lindex $args 0]
    foreach x $args {
	if {$x > $m} {set m $x}
    }
    return $m
}

# Generate a random 256 colors colormap suitable for mandelbrot images
proc createcmap {} {
    set minrange 50
    set passes 5
    set cval [expr {256.0/$passes}]
    for {set i 0} {$i < 256} {incr i} {
	lappend cmap [list 0 0 0]
    }
    while {[incr passes -1] >= 0} {
	while 1 {
	    if {$passes == 0} {
		set start 0
		set end 255
	    } else {
		set start [expr {int(rand()*256)}]
		set end [expr {int(rand()*256)}]
	    }
	    if {abs($start-$end) >= $minrange} {
		if {$start > $end} {
		    set t $start
		    set start $end
		    set end $t
		}
		break
	    }
	}
	set radd [expr {int(rand()*$cval)}]
	set gadd [expr {int(rand()*$cval)}]
	set badd [expr {int(rand()*$cval)}]

	set sinincr [expr {[pi]/(($end-$start)+1)}]
	set maxval 0
	for {set i $start} {$i <= $end} {incr i} {
	    set alpha [expr {sin(($i-$start)*$sinincr)}]
	    foreach {r g b} [lindex $cmap $i] break
	    set r [expr {$r+($radd*$alpha)}]
	    set g [expr {$g+($gadd*$alpha)}]
	    set b [expr {$b+($badd*$alpha)}]
	    set maxval [max $maxval $r $g $b]
	    lset cmap $i [list $r $g $b]
	}
    }
    # Make it a flat list, and normalize the values.
    set normfactor [expr {255.0/$maxval}]
    foreach rgb $cmap {
	foreach {r b g} $rgb break
	foreach var {r b g} {
	    set $var [expr {int([set $var]*$normfactor)}]
	    if {[set $var] > 255} {set $var 255}
	}
	lappend result $r $g $b
    }
    return $result
}
 
proc main {xres yres} {
    # Create the image
    set pixels [mandel $xres $yres 0.435396403 0.367981352 0.451687191 0.380210061]
    for {set i 0} {$i < 256} {incr i} {
	lappend colortab $i $i $i
    }
    set colortab [createcmap]
    GifOptimize colortab pixels
    set gif [GifEncode $xres $yres $colortab $pixels]
    set fd [open mandel.gif w]
    fconfigure $fd -translation binary
    puts -nonewline $fd $gif
    close $fd
    puts "Image saved as mandel.gif"
}

main 320 200
