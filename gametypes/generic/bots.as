/*
Copyright (C) 2009-2010 Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

class AIScriptWeightConfigVarGroup
{
	AIScriptWeightConfigVarGroup @nextSibling;
	const String @name;

    AIScriptWeightConfigVar @childVarsHead;
	AIScriptWeightConfigVarGroup @childGroupsHead;


	AIScriptWeightConfigVarGroup( AIScriptWeightConfigVarGroup @parent, const String @name )
	{
		@this.nextSibling = null;
		@this.name = @name;
		@this.childVarsHead = null;
		@this.childGroupsHead = null;

		if ( @parent != null )
			parent.registerGroup( this );
	}

	void registerVar( AIScriptWeightConfigVar @var )
	{
		@var.nextSibling = @this.childVarsHead;
		@this.childVarsHead = @var;
	}

	void registerGroup( AIScriptWeightConfigVarGroup @group )
	{
		@group.nextSibling = @this.childGroupsHead;
		@this.childGroupsHead = @group;
	}
}

class AIScriptWeightConfigVar
{
	AIScriptWeightConfigVar @nextSibling;
	const String @name;
	float value;
	float minValue;
	float maxValue;
	float defaultValue;

	AIScriptWeightConfigVar( AIScriptWeightConfigVarGroup @parent, const String @name )
	{
		@this.nextSibling = null;
		@this.name = @name;

		if ( @parent != null )
			parent.registerVar( this );
	}

	void getValueProps( float &out value, float &out minValue, float &out maxValue, float &out defaultValue )
	{
		value = this.value;
		minValue = this.minValue;
		maxValue = this.maxValue;
		defaultValue = this.defaultValue;
	}

	void setValue( float value )
	{
		this.value = value;
	}
}

void GENERIC_GetScriptWeightConfigVarValueProps( AIScriptWeightConfigVar @var, float &out value, float &out minValue, float &out maxValue, float &out defaultValue)
{
	var.getValueProps( value, minValue, maxValue, defaultValue );
}

void GENERIC_SetScriptWeightConfigVarValue( AIScriptWeightConfigVar @var, float value )
{
	var.setValue( value );
}
