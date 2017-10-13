# grenadeAnimateBoom.py
# 
# Selected object will be animated along X axis
#

import maya.cmds as cmds
import sys

#----------------------------------------
# constants
#----------------------------------------

DECAY_COEF = 0.75
DISTANCE = 25.0
SPEED = 50.0
SPEED_THRESHOLD = 5.0
OBJECT_RADIUS = 0.1
ARC_HEIGHT = 5.0
ANIMATION_FPS = 30

#----------------------------------------
# functions
#----------------------------------------

# jump (recursive)
def makeJump(pObjectName, pFrame, pCoordX, pHeightRatio, pDistance, pSpeed):

    # stop at speed threshold
    if pSpeed < SPEED_THRESHOLD:
        print 'Object stopped at frame %s at distance %s' %(pFrame, pCoordX)
        return (pFrame, (pCoordX, pDistance*pHeightRatio, cmds.getAttr("%s.translate" %(pObjectName))[0][2]))

    # stop at enemy
    if pCoordX+pDistance > enemyX:
        if pCoordX+pDistance*0.5 < enemyX:
            # draw half curve
            pCoordX += pDistance*0.5
            height = pDistance*pHeightRatio
            if height < OBJECT_RADIUS:
                height = OBJECT_RADIUS
            pFrame += pDistance*0.5*ANIMATION_FPS/pSpeed
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateX', v=pCoordX, itt='linear', ott='linear')
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateY', v=height, itt='flat', ott='flat')
            # draw finale falling
            finDistance = enemyX-pCoordX
            pCoordX = enemyX
            finRatio = finDistance/(pDistance*0.5)
            height -= height*finRatio
            pFrame += finDistance*ANIMATION_FPS/pSpeed
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateX', v=pCoordX, itt='linear', ott='linear')
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateY', v=height, itt='linear', ott='linear')
        else:
            # draw finale rising
            finDistance = enemyX-pCoordX
            pCoordX = enemyX
            finRatio = finDistance/(pDistance*0.5)
            height = pDistance*pHeightRatio*finRatio
            if height < OBJECT_RADIUS:
                height = OBJECT_RADIUS
            pFrame += finDistance*ANIMATION_FPS/pSpeed
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateX', v=pCoordX, itt='linear', ott='linear')
            cmds.setKeyframe(pObjectName, t=pFrame, at='translateY', v=height, itt='linear', ott='linear')
        print 'Object stopped at frame %s at distance %s' %(pFrame, enemyX)
        return (pFrame, (enemyX, height, cmds.getAttr("%s.translate" %(pObjectName))[0][2]))
    
    # arc key
    height = pDistance*pHeightRatio
    if height < OBJECT_RADIUS:
        height = OBJECT_RADIUS
    midFrame = pFrame+pDistance*ANIMATION_FPS*0.5/pSpeed
    cmds.setKeyframe(pObjectName, t=midFrame, at='translateY', v=height, itt='flat', ott='flat')

    # horizontal key
    pFrame += pDistance*ANIMATION_FPS/pSpeed
    pCoordX += pDistance
    cmds.setKeyframe(pObjectName, t=pFrame, at='translateX', v=pCoordX, itt='linear', ott='linear')

    # landing key
    cmds.setKeyframe(pObjectName, t=pFrame, at='translateY', v=OBJECT_RADIUS, itt='linear', ott='linear')

    # next jump
    pDistance *= (DECAY_COEF**2)
    pSpeed *= DECAY_COEF
    return makeJump(pObjectName, pFrame, pCoordX, pHeightRatio, pDistance, pSpeed)

#----------------------------------------
# entry point
#----------------------------------------

print "---- Start grenadeAnimate.py ----\n"

# get objects
selection = cmds.ls(orderedSelection=True, type='transform')
if len(selection) < 3:
    print "Select three objects: grenade, particle and enemy."
    sys.exit()

object1Name = selection[0]
object2Name = selection[1]
object3Name = selection[2]

cmds.cutKey(object1Name)
cmds.cutKey(object1Name, s=True, at='v')

# set first key at 0 frame
timeFrame = 0
coordX = cmds.getAttr('%s.tx' %(object1Name))
coordY = cmds.getAttr('%s.ty' %(object1Name))
cmds.setKeyframe(object1Name, t=timeFrame, at='tx', v=coordX, itt='linear', ott='linear')
cmds.setKeyframe(object1Name, t=timeFrame, at='ty', v=coordY, itt='linear', ott='linear')

# start jumping
heightRatio = ARC_HEIGHT/DISTANCE
enemyX = cmds.getAttr('%s.translate' %(object3Name))[0][0]
boomData = makeJump(object1Name, timeFrame, coordX, heightRatio, DISTANCE, SPEED)

print "\n---- End grenadeAnimate.py ----\n"


# kaboom.py
#
# Creates explosion effect using object1 as center and object2 as particle class
#

import random

random.seed(1875)

#----------------------------------------
# constants
#----------------------------------------

PARTICLES_NUM = 25
EXPLOSION_POW = 50
START_FRM = boomData[0]
EXPL_DURATION = 15
PAR_SCALE = 4

#----------------------------------------
# entry point
#----------------------------------------

print "---- Start kaboom.py ----\n"

'''
# get objects
selection = cmds.ls(orderedSelection=True)
if len(selection) < 2:
    print "Select two objects: center and particle."
    sys.exit()

object1Name = selection[0]
object2Name = selection[1]
'''

cmds.showHidden(object2Name)
# coords = cmds.getAttr("%s.translate" %(object1Name))[0]
coords = boomData[1]
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
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='tx', v=coordsExpl[i][0], itt='linear', ott='linear')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='ty', v=coordsExpl[i][1], itt='linear', ott='linear')
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='tz', v=coordsExpl[i][2], itt='linear', ott='linear')

    # TODO: set keys to scale
    scl = random.uniform(0.3, 2)
    cmds.setKeyframe(parName, t=START_FRM, at='sx', v=scl, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM, at='sy', v=scl, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM, at='sz', v=scl, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+1, at='sx', v=scl*PAR_SCALE, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+1, at='sy', v=scl*PAR_SCALE, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+1, at='sz', v=scl*PAR_SCALE, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sx', v=0, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sy', v=0, itt='linear', ott='linear', s=False)
    cmds.setKeyframe(parName, t=START_FRM+EXPL_DURATION, at='sz', v=0, itt='linear', ott='linear', s=False)

cmds.hide(object2Name)
cmds.setKeyframe(object1Name, t=START_FRM, at='v', v=1)
cmds.setKeyframe(object1Name, t=START_FRM+1, at='v', v=0)
cmds.setKeyframe(particlesGroupName, t=START_FRM-1, at='v', v=0)
cmds.setKeyframe(particlesGroupName, t=START_FRM, at='v', v=1)

print "\n---- End kaboom.py ----\n"

