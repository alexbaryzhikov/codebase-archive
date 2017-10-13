# shotIntercept.py
#
# Takes target and bullet objects and calculates the intercept point
# for bullet to hit target
#

import maya.cmds as cmds
import math
import sys
from maya.OpenMaya import MVector

#---------------------------------------
# constants
#----------------------------------------

BULLET_SPEED = 30.0
TARGET_SPEED = 0.0
TARGET_DIRECTON = 0.0

#----------------------------------------
# entry point
#----------------------------------------

print "---- Start shotIntercept.py ----"

# getting object

selection = cmds.ls(selection=True, type='transform')

if len(selection) < 2:
    print "Select target object and then bullet object."
    sys.exit()
      
targetName = selection[0]
bulletName = selection[1]

print "Target is %s, bullet is %s" %(targetName, bulletName)

newAttName = 'Speed'
if not cmds.objExists('%s.%s' %(targetName, newAttName)):
    print "Creating new attribute %s on %s" %(newAttName, targetName)
    cmds.select(targetName)
    cmds.addAttr(longName=newAttName,
                 attributeType='double', defaultValue=TARGET_SPEED, keyable=True)
else:
    print "%s already has attribute %s" %(targetName, newAttName)

newAttName = 'Direction'
if not cmds.objExists('%s.%s' %(targetName, newAttName)):
    print "Creating new attribute %s on %s" %(newAttName, targetName)
    cmds.select(targetName)
    cmds.addAttr(longName=newAttName,
                 attributeType='double', defaultValue=TARGET_DIRECTON, keyable=True)
else:
    print "%s already has attribute %s" %(targetName, newAttName)

newAttName = 'Speed'
if not cmds.objExists('%s.%s' %(bulletName, newAttName)):
    print "Creating new attribute %s on %s" %(newAttName, bulletName)
    cmds.select(bulletName)
    cmds.addAttr(longName=newAttName,
                 attributeType='double', defaultValue=BULLET_SPEED, keyable=True)
else:
    print "%s already has attribute %s" %(bulletName, newAttName)

#----------------------------------------
# intercept procedures
#----------------------------------------

def getInterceptPos(_vTargetPos, _vTargetSpeed, _vBulletPos, _bulletSpeed):
    # if positions are the same
    if _vTargetPos == _vBulletPos:
        return _vTargetPos
    # convert vectors to relative form
    vTargetRelPos = _vTargetPos - _vBulletPos
    t = getInterceptTime(vTargetRelPos, _vTargetSpeed, _bulletSpeed)
    return _vBulletPos+vTargetRelPos+_vTargetSpeed*t

def getInterceptTime(_vTargetPos, _vTargetSpeed, _bulletSpeed):
    targetDistance = _vTargetPos.length()
    targetSpeed = _vTargetSpeed.length()
    # if the target is resting
    if targetSpeed < 0.001:
        return 0.0
    a = math.pow(targetSpeed, 2) - math.pow(_bulletSpeed, 2)
    # if the target and the bullet have the same speed
    if math.fabs(a) < 0.001:
        t1 = -math.pow(targetDistance, 2)/(_vTargetPos*_vTargetSpeed*2.0)
        if t1 > 0:
            return t1
        else:
            return 0
    # if the target is moving faster/slower then the bullet
    b = _vTargetPos*_vTargetSpeed*2.0
    c = math.pow(targetDistance, 2)
    determinant = math.pow(b, 2)-a*c*4.0
    # if target can't be intercepted
    if determinant < 0:
        return 0
    # if target can be intercepted in 2 points
    if determinant > 0:
        t1 = (-b+math.sqrt(determinant))/(a*2.0)
        t2 = (-b-math.sqrt(determinant))/(a*2.0)
        if t1 > 0:
            if t2 > 0:
                return min(t1, t2)
            return t1
        else:
            return max(t2, 0)
    # if there's only one way to intercept
    if determinant == 0:
        t1 = (-b)/(a*2.0)
        return max(t1, 0)

#----------------------------------------
# calculate intercept point
#----------------------------------------

targetPos = cmds.getAttr('%s.translate' %(targetName))[0]
targetSpeed = cmds.getAttr('%s.Speed' %(targetName))
targetDirection = cmds.getAttr('%s.Direction' %(targetName))
bulletPos = cmds.getAttr('%s.translate' %(bulletName))[0]
bulletSpeed = cmds.getAttr('%s.Speed' %(bulletName))
vTargetPos = MVector(targetPos[0], targetPos[1], targetPos[2])
vTargetSpeed = MVector(math.cos(targetDirection)*targetSpeed,
                       targetPos[1],
                       math.sin(targetDirection)*targetSpeed)
vBulletPos = MVector(bulletPos[0], bulletPos[1], bulletPos[2])
vInterceptPos = getInterceptPos(vTargetPos, vTargetSpeed, vBulletPos, bulletSpeed)
vBulletDirection = vInterceptPos-vBulletPos
vUnit = MVector(1, 0, 0)
bulletDirection = vBulletDirection.angle(vUnit)
if vBulletDirection.z < 0:
    bulletDirection *= -1.0

# create locator at intercept point
cmds.spaceLocator( p=(vInterceptPos.x, vInterceptPos.y, vInterceptPos.z) )    

#----------------------------------------
# animate objects
#----------------------------------------

oldExpressions = cmds.listConnections(selection, type='expression')
if not oldExpressions == []:
    cmds.delete(oldExpressions)
cmds.expression(ae=True, o=targetName, 
                s="tx = python(\"targetPos[0]\")+time*Speed*cos(Direction)")
cmds.expression(ae=True, o=targetName,
                s="tz = python(\"targetPos[2]\")+time*Speed*sin(Direction)")
cmds.expression(ae=True, o=bulletName, 
                s="tx = python(\"bulletPos[0]\")+time*Speed*python(\"math.cos(bulletDirection)\")")
cmds.expression(ae=True, o=bulletName,
                s="tz = python(\"bulletPos[2]\")+time*Speed*python(\"math.sin(bulletDirection)\")")


print "---- Finish shotIntercept.py ----"




