.TH DSTOOL_BROWSER l "9 September 1992"

.SH NAME
dstool_browser
\- a user panel for examining, selecting, highlighting, and deleting 
points from dstool memory objects.
.SH SYNOPSIS
.SS User Panel Interface:
extern Menu_item browser_handler();

extern int browser_field_manager(); 
.SS User Panel Entry:
{"Browser...", browser_handler, browser_field_manager}
.SS Library Name:
browserlib.a
.SH DESCRIPTION
The 
.B dstool_browser
is a contributed user panel optionally installed in 
.B dstool. 
For more information on this panel, please see 
the latex documentation in $DSTOOL/contrib/browser/doc.
.SS dstool Memory Objects
Dstool data is grouped into 
categories of different types (trajectory, fixed points, etc.) and stored 
as individual units called 
.I memory objects.  
Within each memory object, the stored data are
further organized into 
.I flows, 
composed of multiple 
.I trajectories
which represent collections of closely related data 
.I points.
These definitions arise as a result of the most important example of a memory
object \-\- that which stores the orbit data obtained by integrating or 
iterating the dynamical system \-\- but their interpretation in dstool 
is much broader. For more information regarding the organization of memory 
objects, see the dstool Reference Manual.
.SS Panel Items

.B Memory stack setting
\- Allows the user to select a memory object for inspection.

.B Flow numeric text field
\- Allows the user to select a flow from within the memory object.  
The valid range of flow	numbers within the selected memory object is 
displayed to the right of this field.

.B Trajectory numeric text field
\-  Allows the user to select a particular trajectory from the flow.  
The range of trajectories in the current flow object is displayed to the 
right of this field.

.B Point numeric text field
\- Allows the user to select a point from the prescribed trajectory.  
The range of points in the trajectory is displayed to the right of this field.

.B Delete flow command button
\- Selection of this button will delete the selected flow.  Dstool will 
NOT ask for confirmation. The view windows are not automatically updated, 
but the	stored points field on the Command Window is adjusted.

.B Write to stdout command button
\- Selection of this button writes the currently displayed point to 
the standard output device (normally the terminal).

.B  Highlight setting
\- Selecting the 
.I On
option will cause the representation of the current point in the view 
windows to be identified by the centering of a pair of crosshairs on the 
point in each graphical window it is displayed. These crosshairs will 
disappear when the mouse pointer leaves the browser window.

.B Copy to Selected command button
\- Selection of this button will copy the relevant components of the 
current point into the Selected Points panel.

.SH AUTHOR
P. A. Worfolk

Center for Applied Mathematics, Cornell University

Ithaca, New York, 14853

.I paw@macomb.tn.cornell.edu
.SH FILES
$DSTOOL/contrib/browser
.SH SEE ALSO
.B dstool(1)

$DSTOOL/contrib/browser/doc
.SH BUGS

Please send mail to 
.I dstool_bugs@macomb.tn.cornell.edu.
