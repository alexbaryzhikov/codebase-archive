# grenadeAnimate.py
# 
# Selected object will be animated along X axis
#

import maya.cmds as cmds

#---------------------------------------
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
        return
    
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
    makeJump(pObjectName, pFrame, pCoordX, pHeightRatio, pDistance, pSpeed)

#----------------------------------------
# entry point
#----------------------------------------

result = cmds.ls(selection=True, type='transform')
if len(result) > 0:

    # get object and clear all keys
    objectName = result[0]
    cmds.cutKey(objectName)

    # set first key at 0 frame
    timeFrame = 0
    coordX = cmds.getAttr('%s.translateX' %(objectName))
    coordY = cmds.getAttr('%s.translateY' %(objectName))
    cmds.setKeyframe(objectName, t=timeFrame, at='translateX', v=coordX, itt='linear', ott='linear')
    cmds.setKeyframe(objectName, t=timeFrame, at='translateY', v=coordY, itt='linear', ott='linear')

    # start jumping
    heightRatio = ARC_HEIGHT/DISTANCE
    makeJump(objectName, timeFrame, coordX, heightRatio, DISTANCE, SPEED)

else:
    print 'Select an object.'
    
