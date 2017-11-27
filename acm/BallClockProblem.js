'use strict'

// http://www.chilton.com/~jimw/ballclk.html
class BallClock {
  constructor(numberOfBalls) {
    this.ballQueue = []
    this.ballQueueOriginal = []
    this.trackMinutes = []
    this.trackFiveMinutes = []
    this.trackHours = []

    for (var i = 1; i <= numberOfBalls; i++) {
      this.ballQueueOriginal.push(i)
      this.ballQueue.push(i)
    }
  }

  tick() {
    var ball = this.ballQueue.shift()
    this.addToMinuteTrack(ball)
  }

  flush(track) {
    while (track.length) {
      this.ballQueue.push(track.pop())
    }
  }

  dump() {
    console.log({
      Min: this.trackMinutes,
      FiveMin: this.trackFiveMinutes,
      Hour: this.trackHours,
      Main: this.ballQueue,
    })
  }

  cycleCompleted() {
    return this.sameArray(this.ballQueue, this.ballQueueOriginal)
  }

  sameArray(one, two) {
    if (one.length !== two.length) {
      return false
    }

    for (var i = 0, len = one.length; i < len; i++) {
      if (one[i] !== two[i]) {
        return false
      }
    }

    return true
  }

  addToMinuteTrack(ball) {
    if (this.trackMinutes.length < 4) {
      this.trackMinutes.push(ball)
    } else {
      this.flush(this.trackMinutes)
      this.addToFiveMinuteTrack(ball)
    }
  }

  addToFiveMinuteTrack(ball) {
    if (this.trackFiveMinutes.length < 11) {
      this.trackFiveMinutes.push(ball)
    } else {
      this.flush(this.trackFiveMinutes)
      this.addToHourTrack(ball)
    }
  }

  addToHourTrack(ball) {
    if (this.trackHours.length == 11) {
      this.flush(this.trackHours)
      this.ballQueue.push(ball)
    } else {
      this.trackHours.push(ball)
    }
  }
}

function main() {
  if (process.argv.length < 3) {
    console.error("Usage: %s <numberOfBalls> [dumpAt]", process.argv[1])
    process.exit(1)
  }

  var numberOfBalls = +process.argv[2]
  var dumpAt = +process.argv[3]

  if (numberOfBalls < 27 || numberOfBalls > 127) {
    console.error("Number of balls must be between 27 and 127")
    process.exit(1)
  }

  var clock = new BallClock(numberOfBalls)

  for (var i = 0; i < 1000000000000000000000000; i++) {
    clock.tick();

    if (i + 1 === dumpAt) {
      clock.dump()
      break
    }

    if (clock.cycleCompleted()) {
      console.log("%s balls cycle after %s days.", numberOfBalls, Math.ceil(i / 60 / 24))
      console.log("Completed in %s milliseconds (%s seconds)", i * 60 * 1000, i * 60)
      break
    }
  }
}

main();
