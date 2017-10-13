# dotProduct.py
#
# Takes translates of two objects as vectors and
# calculates the dot product these vectors
#


import maya.cmds as cmds
import operator
import sys
from maya.OpenMaya import MVector

selection = cmds.ls(selection=True, type='transform')

if len(selection) < 2:
    print "Select target object and then bullet object."
    sys.exit()
      
obj1 = selection[0]
obj2 = selection[1]

xP = cmds.getAttr('%s.translate' %(obj1))[0]
yP = cmds.getAttr('%s.translate' %(obj2))[0]

x = MVector(xP[0], xP[1], xP[2])
y = MVector(yP[0], yP[1], yP[2])

print x*y

