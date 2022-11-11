//go:generate stringer -type=Rank
package main

import (
	"fmt"

	"github.com/google/uuid"
)

type Player struct {
	Id   uuid.UUID
	Rank Rank
}

func NewPlayer(rank Rank) Player {
	return Player{
		Id:   uuid.New(),
		Rank: rank,
	}
}

func (p Player) String() string {
	return fmt.Sprintf("player(%s, %s)", p.Id.String(), p.Rank.String())
}

type Rank int

const (
	Beginner Rank = iota
	Amateur
	Skilled
	Great
	Advanced
	Expert
)

func (player Player) IsValidOpponent(opponent Player) bool {
	return player.Rank == opponent.Rank
}
