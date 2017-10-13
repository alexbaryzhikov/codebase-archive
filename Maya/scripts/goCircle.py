# goCircle.py
#
# Makes object circling around its Z axis
#

import maya.cmds as cmds
import math
import sys

#---------------------------------------
# constants
#----------------------------------------

RADIUS = 3

#----------------------------------------
# entry point
#----------------------------------------

print "---- Start goCyrcle.py ----"

# getting object
selection = cmds.ls(selection=True, type='transform')

if len(selection) == 0:
    print "Select an object."
    sys.exit()
      
objName = selection[0]
print "Animating object: %s" %(objName)

# creating new attribute
newAttName = 'Radians'
if not cmds.objExists('%s.%s' %(objName, newAttName)):
    print "Creating new attribute %s on %s" %(newAttName, objName)
    cmds.select(objName)
    cmds.addAttr(longName=newAttName, shortName='rad',
                 attributeType='double', defaultValue=0, keyable=True)
else:
    print "%s already has attribute %s" %(objName, newAttName)

# creating expressions
cmds.expression(ae=True, o=objName, s="tx = cos(time)*python(\"RADIUS\")")
cmds.expression(ae=True, o=objName, s="ty = sin(time)*python(\"RADIUS\")")
cmds.expression(ae=True, o=objName, s="rad = time")

