// by ficool2

const IN_JUMP = 2
const IN_FORWARD = 8
const IN_BACK = 16
const IN_MOVELEFT = 512
const IN_MOVERIGHT = 1024

const FL_DUCKING = 2

const MASK_PLAYERSOLID_LADDER = 81931 // CONTENTS_SOLID|CONTENTS_MOVEABLE|CONTENTS_PLAYERCLIP|CONTENTS_WINDOW|CONTENTS_GRATE

const cl_forwardspeed = 450
const cl_backspeed = 450
const cl_sidespeed = 450

const MOVETYPE_NONE = 0
const MOVETYPE_WALK = 2

const MAX_CLIMB_SPEED = 200.0

const sv_ladder_dampen = 0.2
const sv_ladder_angle = -0.707

const LADDER_FOOTSTEP_LEFT = "Ladder.StepLeft"
const LADDER_FOOTSTEP_RIGHT = "Ladder.StepRight"

::vec3_origin <- Vector(0, 0, 0)
::vec3_up <- Vector(0, 0, 1)

::LadderCull <- function(origin, mins, maxs)
{
	foreach (ladder in Ladders)
	{
		if (ladder.dynamic)
		{
			local dist_sqr = (origin - ladder.entity.GetOrigin()).LengthSqr()
			if (dist_sqr <= ladder.radius_sqr)
			{
				return false
			}
		}
		else
		{
			local ladder_mins = ladder.mins
			local ladder_maxs = ladder.maxs
			if ((mins.x <= ladder_maxs.x && maxs.x >= ladder_mins.x) &&
				(mins.y <= ladder_maxs.y && maxs.y >= ladder_mins.y) &&
				(mins.z <= ladder_maxs.z && maxs.z >= ladder_mins.z))
			{
				return false
			}
		}
	}
	
	return true
}

::LadderPlayerTryMove <- function(origin, velocity, vecMins, vecMaxs, dt)
{
	local start_origin = origin * 1.0
	
	local time_left = dt
	local allFraction = 0.0
	local original_velocity = velocity * 1.0
	local primal_velocity = velocity * 1.0
	local planes = []
	local numplanes = 0
	
	local pm =
	{
		mask		= MASK_PLAYERSOLID_LADDER
		hullmin		= vecMins
		hullmax		= vecMaxs
		ignore		= self
	}	
	
	// ficool2: reduced bumpcount to 2 for optimization
	for (local bumpcount = 0; bumpcount < 2; bumpcount++)
	{
		if (!velocity.LengthSqr())
			break

		local end = origin + velocity * time_left
		
		pm.start	<- origin
		pm.end		<- origin + velocity * time_left
		pm.allsolid	<- false
		TraceHull(pm)
		
		allFraction += pm.fraction
		
		if (pm.allsolid)
		{
			velocity *= 0.0
			return
		}
		
		if (pm.fraction > 0.0)
		{
			origin = pm.endpos
			original_velocity = velocity * 1.0
			planes.clear()
			numplanes = 0
		}
		
		if (pm.fraction == 1.0)
			break
			
		time_left -= time_left * pm.fraction
		
		if (numplanes >= 5)
		{
			velocity *= 0.0
			break
		}
		
		planes.append(pm.plane_normal)
		++numplanes
		
		local i = 0
		for (; i < numplanes; i++)
		{
			local normal = planes[i]
			local blocked = original_velocity.Dot(normal)
			local backoff = blocked
			velocity = original_velocity - normal * backoff
			local adjust = velocity.Dot(normal)
			if (adjust < 0.0)
				velocity -= normal * adjust
			
			local j = 0
			for (; j < numplanes; j++)
			{
				if (i != j)
					if (velocity.Dot(planes[j]) < 0.0)
						break
			}
			if (j == numplanes)
				break
		}

		if (i == numplanes)
		{
			if (numplanes != 2)
			{
				velocity *= 0.0
				break
			}
			
			local dir = planes[0].Cross(planes[1])
			dir.Norm()
			velocity = dir * dir.Dot(velocity)
		}
		
		if (velocity.Dot(primal_velocity) <= 0.0)
		{
			velocity *= 0.0
			break
		}
	}
	
	if (!allFraction)
		velocity *= 0.0
	
	// ficool2 hack: unknown stuck issue, double check valid position
	local dest = origin + velocity * dt
	pm.start		<- start_origin
	pm.end			<- dest
	pm.startsolid	<- false
	TraceHull(pm)
	
	if (!pm.startsolid)
	{
		self.SetAbsOrigin(pm.endpos)
		self.SetAbsVelocity(velocity)
	}
	else
	{
		self.SetAbsVelocity(vec3_origin)
	}
}

::LadderPlayerThink <- function()
{
	if (!self.IsAlive() || self.IsNoclipping())
		return -1
		
	local nButtons = NetProps.GetPropInt(self, "m_nButtons")
	
	local flForwardMove = 0.0
	local flSideMove = 0.0
	if (nButtons & IN_FORWARD)
		flForwardMove += cl_forwardspeed
	if (nButtons & IN_BACK)
		flForwardMove -= cl_forwardspeed
	if (nButtons & IN_MOVERIGHT)
		flSideMove += cl_sidespeed
	if (nButtons & IN_MOVELEFT)
		flSideMove -= cl_sidespeed
		
	if (!m_bOnLadder && !flForwardMove && !flSideMove)
		return -1.0
	
	local vecAbsOrigin = self.GetOrigin()
	local vecMins = self.GetPlayerMins()
	local vecMaxs = self.GetPlayerMaxs()
	
	if (LadderCull(vecAbsOrigin, vecMins, vecMaxs))
		return -1.0
	
	local angEyes = self.EyeAngles()
	local vecForward = angEyes.Forward()
	local vecRight = angEyes.Left()
	
	local wishdir
	if (m_bOnLadder)
	{
		wishdir = m_vecLadderNormal * -1.0
	}
	else
	{
		wishdir = vecForward * flForwardMove + vecRight * flSideMove
		wishdir.Norm()
	}
	
	local pm =
	{
		start		= vecAbsOrigin
		end			= vecAbsOrigin + wishdir * (m_bOnLadder ? 10.0 : 2.0)
		mask		= MASK_PLAYERSOLID_LADDER
		hullmin		= vecMins
		hullmax		= vecMaxs
		ignore		= self
	}
	TraceHull(pm)
	
	if (pm.fraction == 1.0 || pm.enthit.GetClassname() != "func_ladder")
	{
		if (m_bOnLadder)
		{
			self.SetMoveType(MOVETYPE_WALK, 0)
			m_bOnLadder = false
		}
		return -1
	}

	m_vecLadderNormal = pm.plane_normal
	m_bOnLadder = true
	self.SetMoveType(MOVETYPE_NONE, 0)
	
	local forwardSpeed = 0.0
	local rightSpeed = 0.0
	if (nButtons & IN_BACK)
		forwardSpeed -= MAX_CLIMB_SPEED
	if (nButtons & IN_FORWARD)
		forwardSpeed += MAX_CLIMB_SPEED
	if (nButtons & IN_MOVELEFT)
		rightSpeed -= MAX_CLIMB_SPEED
	if (nButtons & IN_MOVERIGHT)
		rightSpeed += MAX_CLIMB_SPEED
		
	local dt = FrameTime()
	
	if (m_flStepSoundTime > 0.0)
	{
		m_flStepSoundTime -= 1000.0 * dt
		if (m_flStepSoundTime < 0)
			m_flStepSoundTime = 0
	}

	if (m_flStepSoundTime <= 0.0)
	{
		if (self.GetAbsVelocity().Length() >= 60.0)
		{
			EmitSoundEx(
			{
				sound_name = m_bStepside ? LADDER_FOOTSTEP_LEFT : LADDER_FOOTSTEP_RIGHT,
				volume = (self.GetFlags() & FL_DUCKING) ? 0.325 : 0.5,
				entity = self
			})
			
			m_bStepside = !m_bStepside
		}
		
		m_flStepSoundTime = 450.0
	}
		
	if (nButtons & IN_JUMP)
	{
		m_bOnLadder = false
		local vecNewVelocity = m_vecLadderNormal * 270.0
		self.SetMoveType(MOVETYPE_WALK, 0)
		self.SetAbsVelocity(vecNewVelocity)
	}
	else
	{
		if (forwardSpeed != 0.0 || rightSpeed != 0.0)
		{
			local velocity = vecForward * forwardSpeed + vecRight * rightSpeed
			local perp = vec3_up.Cross(m_vecLadderNormal)
			perp.Norm()
			
			local normal = velocity.Dot(m_vecLadderNormal)
			local cross = m_vecLadderNormal * normal
			local lateral = velocity - cross
			local tmp = m_vecLadderNormal.Cross(perp)
			
			local tmpDist = tmp.Dot(lateral)
			local perpDist = perp.Dot(lateral)
			local angleVec =  perp * perpDist + cross
			angleVec.Norm()
			
			if (angleVec.Dot(m_vecLadderNormal) < sv_ladder_angle)
				lateral = (tmp * tmpDist) + (perp * (sv_ladder_dampen * perpDist))	
			
			local vecNewVelocity = lateral + tmp * (normal * -1.0)
			
			if (normal > 0.0)
			{
				local onFloor = false
				if (NetProps.GetPropEntity(self, "m_hGroundEntity"))
				{
					onFloor = true
				}
				else
				{
					local vecFloor = vecAbsOrigin * 1.0
					vecFloor.z += pm.hullmin.z - 1.0
					
					pm.start = vecFloor
					pm.end = vecFloor
					TraceLineEx(pm)
					onFloor = pm.hit
				}
					
				if (onFloor)
				{
					vecNewVelocity += m_vecLadderNormal * MAX_CLIMB_SPEED
				}
			}
			
			LadderPlayerTryMove(vecAbsOrigin, vecNewVelocity, vecMins, vecMaxs, dt)
		}
		else
		{
			self.SetAbsVelocity(vec3_origin)
		}
	}
	
	return -1.0
}

function Precache()
{
	PrecacheScriptSound(LADDER_FOOTSTEP_LEFT)
	PrecacheScriptSound(LADDER_FOOTSTEP_RIGHT)
}

function OnPostSpawn()
{
	self.KeyValueFromString("classname", "func_ladder")
	
	local bloat = Vector(8.0, 8.0, 8.0)
	local mins = self.GetBoundingMins() - bloat
	local maxs = self.GetBoundingMaxs() + bloat
	local radius_sqr = (maxs - mins).LengthSqr()
	local dynamic = self.GetMoveParent() != null
	
	Ladders.append
	({
		entity		= self
		radius_sqr	= radius_sqr
		mins		= mins
		maxs		= maxs
		dynamic		= dynamic
	})
}

if (!("LadderEvents" in getroottable()))
{
	::Ladders <- []
	
	::LadderEvents <- 
	{
		function OnGameEvent_stats_resetround(params)
		{
			Ladders.clear()
		}
		
		function OnGameEvent_player_spawn(params)
		{
			local player = GetPlayerFromUserID(params.userid)
			if (!player)
				return

			player.ValidateScriptScope()
			local scope = player.GetScriptScope()
			scope.m_bOnLadder		<- false
			scope.m_vecLadderNormal	<- Vector()
			scope.m_flStepSoundTime	<- 0.0
			scope.m_bStepside		<- false
			
			AddThinkToEnt(player, "LadderPlayerThink")
		}	
	}
	__CollectGameEventCallbacks(LadderEvents)
}