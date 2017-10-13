# kaboom.py
#
# Creates explosion effect using object1 as center and object2 as particle class
#

import maya.cmds as cmds
import random
import sys

random.seed(1875)

#---------------------------------------
# constants
#----------------------------------------

PARTICLES_NUM = 25
EXPLOSION_POW = 50
START_FRM = 1
EXPL_DURATION = 15
PAR_SCALE = 4

#----------------------------------------
# entry point
#----------------------------------------

print "---- Start kaboom.py ----\n"

# get object1
selection = cmds.ls(orderedSelection=True)
if len(selection) < 2:
    print "Select two objects: center and particle."
    sys.exit()

object1Name = selection[0]
object2Name = selection[1]
cmds.showHidden(object2Name)
coords = cmds.getAttr("%s.translate" %(object1Name))[0]
print object1Name
print object2Name
print coords

particlesGroupName = cmds.group(empty=True, name="particlesGrp#")
coordsExpl = []

for i in range(0, PARTICLES_NUM):

    # generate coords
    x = coords[0]+random.uniform(-EXPLOSION_POW, EXPLOSION_POW)
    y = coords[1]+random.uniform(0, EXPLOSION_POW*2)
    z = coords[2]+random.uniform(-EXPLOSION_POW, EXPLOSION_POW)
    coordsExpl.insert(i, (x, y, z))

    # create particle    
    parName = cmds.instance(object2Name, n=object2Name+"_inst#")[0]
    cmds.xform(parName, centerPivots=True)
    cmds.move(coords[0], coords[1], coords[2], parName)
    cmds.parent(parName, particlesGroupName)
    x = random.uniform(0, 360)
    y = random.uniform(0, 360)
    z = random.uniform(0, 360)
    cmds.rotate(x, y, z, parName)

    # TODO: set keys to translate
    cmds.setKeyframe(parName, t=START_FRM, at='tx', v=coords[0], itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM, at='ty', v=coords[1], itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM, at='tz', v=coords[2], itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='tx', v=coordsExpl[i][0], itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='ty', v=coordsExpl[i][1], itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='tz', v=coordsExpl[i][2], itt='flat', ott='flat')

    # TODO: set keys to scale
    scl = random.uniform(0.3, 2)
    cmds.setKeyframe(parName, t=START_FRM, at='sx', v=scl, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM, at='sy', v=scl, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM, at='sz', v=scl, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+5, at='sx', v=scl*PAR_SCALE, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+5, at='sy', v=scl*PAR_SCALE, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+5, at='sz', v=scl*PAR_SCALE, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sx', v=0, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sy', v=0, itt='flat', ott='flat')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sz', v=0, itt='flat', ott='flat')

cmds.setKeyframe(object1Name, t=START_FRM, at='v', v=1, itt='flat', ott='flat')
cmds.setKeyframe(object1Name, t=START_FRM+1, at='v', v=0, itt='flat', ott='flat')
cmds.hide(object2Name)

print "\n---- End kaboom.py ----\n"




