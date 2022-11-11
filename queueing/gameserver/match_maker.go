package main

import (
	"fmt"
	"log"
	"sync"
)

type Match struct {
	p1 Player
	p2 Player
}

func (m Match) String() string {
	return fmt.Sprintf("%s v. %s", m.p1.String(), m.p2.String())
}

type matchMakerState int

const (
	disabled matchMakerState = iota
	enabled
)

type MatchMaker struct {
	Matches chan Match

	limit int
	state matchMakerState
	queue []Player
	mux   sync.Mutex
}

func NewMatchMaker(limit int) *MatchMaker {
	return &MatchMaker{
		Matches: make(chan Match, limit),
		limit:   limit,
		state:   disabled,
		mux:     sync.Mutex{},
	}
}

func (mm *MatchMaker) QueuePlayer(player Player) error {
	mm.mux.Lock()
	defer mm.mux.Unlock()

	log.Printf("%s wants to play\n", player)
	if len(mm.queue) >= mm.limit {
		return &MatchMakerQueueFull{player}
	}

	// Find match right away
	for i, opponent := range mm.queue {
		if player.IsValidOpponent(opponent) {
			mm.queue = append(mm.queue[:i], mm.queue[i+1:]...)
			mm.Matches <- Match{player, opponent}
			return nil
		}
	}

	// No opponent was found, add player to queue
	mm.queue = append(mm.queue, player)
	log.Printf("queueing %s, %d players are waiting\n", player, len(mm.queue))
	return nil
}

type MatchMakerQueueFull struct {
	Player Player
}

func (err *MatchMakerQueueFull) Error() string {
	return fmt.Sprintf("unable to queue %s, match maker queue is full", err.Player)
}
