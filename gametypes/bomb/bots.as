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

void BOTS_OnBombDropped()
{
	// Add a goal entity for the bombModel entity
	AI::AddNavEntity( bombModel, AI_NAV_REACH_AT_TOUCH );

    AI::RemoveAllObjectiveSpots();

    // Defending team should protect the bomb from being picked up
    AIDefenceSpot defenceSpot( 0, bombModel, 1024.0f );
    defenceSpot.minDefenders = 5;
    defenceSpot.maxDefenders = 999;
    AI::AddDefenceSpot( defendingTeam, defenceSpot );

    // Force all defenders to reach the spot by triggering an alert
	// Note: the alert duration is fairly low as usually spots are close to defender spawns
	// so they don't stick to their assigned spot when an actual alert is called
    AI::DefenceSpotAlert( defendingTeam, 0, 1.0f, uint(10000) );

	// Attackers should pick the bomb.
	AIOffenseSpot offenseSpot( 0, bombModel );
	offenseSpot.minAttackers = 5;
	offenseSpot.maxAttackers = 999;
	AI::AddOffenseSpot( attackingTeam, offenseSpot );
}

void BOTS_OnBombPicked()
{
	// Tell attackers that the bomb has been picked up
	AI::NavEntityReached( bombModel );

	// Remove the nav entity that corresponds to the model
	AI::RemoveNavEntity( bombModel );

	// Clean objectives
	AI::RemoveAllObjectiveSpots();

	// Defenders should start everything
	BOTS_StartDefendingSites();

	if( @bombCarrier.client.getBot() != null )
	{
		@BOMB_BOTS_SITE = BOTS_PickAttackSite( bombCarrier );
	}
}

void BOTS_OnBombInPlace( cBombSite @site )
{
	// Tell attackers they have reached the site
    AI::NavEntityReached( site.indicator );

    // Add a nav entity for the bomb model.
	AI::AddNavEntity( bombModel, AI_NAV_REACH_ON_EVENT );

	// Remove old defending team defence spots
    AI::RemoveAllObjectiveSpots();
    
	// Defending team should prevent planting
	AIDefenceSpot defenceSpot( 0, bombModel, 1024.0f );
    defenceSpot.minDefenders = 5;
    defenceSpot.maxDefenders = 999;
    AI::AddDefenceSpot( defendingTeam, defenceSpot );

    // Force all defenders to reach the defence spot by forcing an alert for 15 seconds
	// Note that the duration is significantly larger so bots from far spots can reach the bomb
    AI::DefenceSpotAlert( defendingTeam, 0, 1.0f, uint(30000) );

	// Attacking team should try planting
	AIOffenseSpot offenseSpot( 0, bombModel );
	offenseSpot.minAttackers = 5;
	offenseSpot.maxAttackers = 999;
	AI::AddOffenseSpot( attackingTeam, offenseSpot );
}

void BOTS_OnBombArmed()
{
	// Tell attackers they have reached the nav entity
	AI::NavEntityReached( bombModel );
	
    AI::RemoveAllObjectiveSpots();

	// Add a defence spot for attacking team
    AIDefenceSpot defenceSpot( 0, bombModel, 1024.0f );
    defenceSpot.minDefenders = 5;
    defenceSpot.maxDefenders = 999;
    AI::AddDefenceSpot( attackingTeam, defenceSpot );

    // Force all attackers to reach the defence spot by forcing an alert for 30 seconds
	// Same notes on duration as for dropping bomb apply for this case as well.
    AI::DefenceSpotAlert( attackingTeam, 0, 1.0f, uint(30000) );

	// Defenders should try defusing
	AIOffenseSpot offenseSpot( 0, bombModel );
	offenseSpot.minAttackers = 5;
	offenseSpot.maxAttackers = 999;
	AI::AddOffenseSpot( defendingTeam, offenseSpot );
}

void BOMB_UpdateBotsExtraGoals()
{
	// Just clear overridden entity weigths in this case
	// Everything else is managed by offense/defence spots native logic
	if( bombState != BOMBSTATE_CARRIED )
	{
		for ( int i = 0; i < maxClients; ++i )
		{
			Client @client = @G_GetClient( i );
			Bot @bot = @client.getBot();
	        if ( @bot != null )
				bot.clearOverriddenEntityWeights();
		}
		return;
	}

	// if a bot carries a bomb and there is an attack site on a map
	if( @bombCarrier.client.getBot() != null && @BOMB_BOTS_SITE != null )
	{
		for( int i = 0; i < maxClients; ++i )
		{
			Client @client = @G_GetClient( i );
			Bot @bot = @client.getBot();
			if ( @bot == null )
				continue;

			bot.clearOverriddenEntityWeights();

			Entity @ent = client.getEnt();
			if( @bombCarrier == @ent )
			{
				bot.overrideEntityWeight( @BOMB_BOTS_SITE.indicator, 12.0f );
				continue;
			}

			// return to the carrier, don't push
			if( ent.origin.distance( bombCarrier.origin ) > 384.0f )
			{
				bot.overrideEntityWeight( bombCarrier, 12.0f );
				continue;
			}

			// advance a bit to the site
			bot.overrideEntityWeight( @BOMB_BOTS_SITE.indicator, 12.0f );
		}
		return;
	}

	// if a player carriers the bomb, just follow him.
	// at this moment we can't know what site is chosen by a players
	// (until teamchat listening module is implemented)
	for( int i = 0; i < maxClients; ++i )
	{
		Client @client = @G_GetClient( i );
		Bot @bot = @client.getBot();
		if( @bot == null )
			continue;

		bot.clearOverriddenEntityWeights();

		// prevent sticking to carrier and blocking
		if( client.getEnt().origin.distance( bombCarrier.origin ) > 256.0f )
			bot.overrideEntityWeight( bombCarrier, 12.0f );
	}
}

cBombSite @BOTS_PickAttackSite( Entity @botCarrier )
{
    String siteLetter = SITE_LETTERS[ ( ( random() < 0.5f ) ? 0 : 1 ) ];
	Bot @bot = botCarrier.client.getBot();

	// first pass: try picking site for letter checking its reachability
	cBombSite @letterSite = null;
	for ( cBombSite @site = @siteHead; @site != null; @site = @site.next )
    {
        if ( site.letter == siteLetter )
		{
			@letterSite = @site;
			if ( bot.checkTravelTimeMillis( botCarrier.origin, site.indicator.origin, true ) > 0 )
				return @site;
		}
    }

	// second pass: try picking any reachable site
	for ( cBombSite @site = @siteHead; @site != null; @site = @site.next )
	{
		if ( bot.checkTravelTimeMillis( botCarrier.origin, site.indicator.origin, true ) > 0 )
			return @site;
	}

	// return the site for letter (if it is present, null otherwise)
    // in hope it becomes reachable (e.g. if the carrier is in air now)
	return @letterSite;
}

void BOTS_SetupNewRound()
{
	G_Print( "Setup new round\n" );

	AI::RemoveAllObjectiveSpots();
    
    // Setup initial defence spots for current defenders
    BOTS_StartDefendingSites();

	if( @bombCarrier == null )
	{
		G_Print( "Warning: no bomb carrier is found\n" );
	}

    // Clear all external entity weights just to ensure everything is reset.
	// Check whether some bot is a bomb carrier at the same time.
	Entity @botCarrier = null;
    for ( int i = 0; i < maxClients; ++i )
    {
		Client @client = @G_GetClient( i );
        Bot @bot = @client.getBot();
        if ( @bot == null )
			continue;

        bot.clearOverriddenEntityWeights();

		Entity @ent = client.getEnt();
		if ( @ent == @bombCarrier )
			@botCarrier = @ent;
    }
    

	if ( @botCarrier != null )
	{
		@BOMB_BOTS_SITE = @BOTS_PickAttackSite( botCarrier );
	}
}

// Defence spots have these id's:
// 0 - special defence spot that corresponds to the bomb model
// 1, ... - spots that correspond to sites

void BOTS_StartDefendingSites()
{    
	G_Print( "StartDefendingAllSites()\n" );

    int spotId = 1;
	for ( cBombSite @site = @siteHead; @site != null; @site = @site.next )
    {
        AIDefenceSpot defenceSpot( spotId, site.indicator, 1024.0f );
        // Allow to leave the spot to defend the one being attacked now.        
        defenceSpot.minDefenders = 0;
        // This value will be clamped to a maximum supported one.
        defenceSpot.maxDefenders = 999;
        defenceSpot.regularEnemyAlertScale = 1.5f;
        defenceSpot.carrierEnemyAlertScale = 5.0f;
        AI::AddDefenceSpot( defendingTeam, defenceSpot );
        // Force all defenders available for the spot to reach the spot
        AI::DefenceSpotAlert( defendingTeam, spotId, 0.5f, uint(15000) );
        spotId++;
    }
}


