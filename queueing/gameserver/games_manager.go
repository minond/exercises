package main

import "log"

type GamesManager struct {
	limit         int
	activeMatches []Match
	matchesC      <-chan Match
	closed        chan struct{}
}

func NewGamesManager(limit int, matchesC <-chan Match) *GamesManager {
	return &GamesManager{
		limit:    limit,
		matchesC: matchesC,
		closed:   make(chan struct{}),
	}
}

func (games *GamesManager) Start() {
	log.Println("starting games manager")
	for {
		select {
		case match := <-games.matchesC:
			games.activeMatches = append(games.activeMatches, match)
			log.Printf("got a match, managing %d games\n", len(games.activeMatches))

			if len(games.activeMatches) >= games.limit {
				log.Println("game manager is full, not accepting new games")
				games.Stop()
			}

		case <-games.closed:
			return
		}
	}
}

func (games GamesManager) Stop() {
	log.Println("stopping games manager")
	close(games.closed)
}
