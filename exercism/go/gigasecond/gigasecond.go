// Package gigasecond gives you everything you need for working with
// gigaseconds.
package gigasecond

import "time"

// Gigasecond is 10^9 (1,000,000,000) seconds.
const Gigasecond = time.Second * 1000000000

// AddGigasecond adds one gigasecond to the given time.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(Gigasecond)
}
