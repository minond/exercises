package main

import (
	"log"
	"math/rand"
	"os"
	"os/signal"
	"time"
)

func main() {
	matchMaker := NewMatchMaker(1000)
	games1 := NewGamesManager(10, matchMaker.Matches)
	games2 := NewGamesManager(10, matchMaker.Matches)

	go func() {
		queuePlayer := time.Tick(time.Millisecond * 100)

		for {
			select {
			case <-queuePlayer:
				player := NewPlayer(Rank(rand.Intn(int(Expert))))

				if err := matchMaker.QueuePlayer(player); err != nil {
					log.Printf("error queueing %s: %s\n", player, err)
				}
			}
		}
	}()

	go games1.Start()
	go games2.Start()

	exit := make(chan bool, 1)
	sig := make(chan os.Signal, 1)
	signal.Notify(sig, os.Interrupt)
	go func() {
		select {
		case <-sig:
			games1.Stop()
			games2.Stop()
			exit <- true
		}
	}()
	<-exit
}
