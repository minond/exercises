package space

type Planet string

const earthYear = 31557600

var conversions = map[Planet]float64{
	"Earth":   1,
	"Mercury": 0.2408467,
	"Venus":   0.61519726,
	"Mars":    1.8808158,
	"Jupiter": 11.862615,
	"Saturn":  29.447498,
	"Uranus":  84.016846,
	"Neptune": 164.79132,
}

// Age takes an age in seconds and calculate how old someone would be on the
// given planet.
func Age(age float64, planet Planet) float64 {
	conv, ok := conversions[planet]
	if !ok {
		return -1
	}
	return age / (conv * earthYear)
}
