# Solar System Simulation - Fortran + WebAssembly

A browser-based solar system simulation where all orbital mechanics are computed by a Fortran module compiled to WebAssembly. JavaScript only handles rendering (canvas) and user input.

## Features

- Real elliptical (Keplerian) orbits for the Sun and eight planets
- Major moons (Earth's Moon, Jupiter's Galilean moons, Saturn's largest, etc.)
- Fading trail behind each planet, showing recent motion
- Live stats table: revolution count and constant orbital speed (km/s)
- Mouse wheel zoom (cursor-centered) and drag to pan
- Playback speed control and pause