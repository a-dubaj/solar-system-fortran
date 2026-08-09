! orbital_mechanics.f90
! Circular-orbit approximation of the Sun and the eight planets.
! Exposed to JavaScript via ISO C bindings once compiled to WebAssembly.
!
! Data source (semi-major axis in AU, orbital period in days):
! standard textbook values, sufficient for a visually accurate simulation.

module orbital_data
    implicit none

    integer, parameter :: n_bodies = 9   ! Sun (index 1) + 8 planets (2-9)

    real(8), parameter :: pi = 3.14159265358979323846d0

    ! Index: 1=Sun, 2=Mercury, 3=Venus, 4=Earth, 5=Mars,
    !        6=Jupiter, 7=Saturn, 8=Uranus, 9=Neptune
    real(8), parameter :: semi_major_axis_au(9) = &
        [0.0d0, 0.387d0, 0.723d0, 1.000d0, 1.524d0, &
         5.203d0, 9.537d0, 19.191d0, 30.069d0]

    real(8), parameter :: orbital_period_days(9) = &
        [1.0d0, 87.969d0, 224.701d0, 365.256d0, 686.980d0, &
         4332.589d0, 10759.22d0, 30688.5d0, 60182.0d0]

    ! Relative body radius in km, used to keep size proportions realistic
    real(8), parameter :: body_radius_km(9) = &
        [696340.0d0, 2439.7d0, 6051.8d0, 6371.0d0, 3389.5d0, &
         69911.0d0, 58232.0d0, 25362.0d0, 24622.0d0]

    ! --- Moons ---
    ! Simplified selection of major moons per body (up to 4 each).
    ! Orbital periods are real (days); display distance from the planet
    ! is handled in JS at an exaggerated scale, since true moon-planet
    ! distances are far too small to render at the same scale as the
    ! planets' distances from the Sun.
    integer, parameter :: max_moons = 4

    ! moon_count: 1=Sun, 2=Mercury, 3=Venus, 4=Earth, 5=Mars,
    !             6=Jupiter, 7=Saturn, 8=Uranus, 9=Neptune
    integer, parameter :: moon_count(9) = [0, 0, 0, 1, 2, 4, 4, 4, 1]

    ! Orbital period of each moon, in days. Unused slots are 0.
    real(8), parameter :: moon_period_days(4, 9) = reshape( &
        [0.0d0,   0.0d0,  0.0d0,  0.0d0, &   ! Sun (no moons)
         0.0d0,   0.0d0,  0.0d0,  0.0d0, &   ! Mercury
         0.0d0,   0.0d0,  0.0d0,  0.0d0, &   ! Venus
         27.32d0, 0.0d0,  0.0d0,  0.0d0, &   ! Earth: Moon
         0.319d0, 1.263d0,0.0d0,  0.0d0, &   ! Mars: Phobos, Deimos
         1.769d0, 3.551d0,7.155d0,16.689d0, &! Jupiter: Io, Europa, Ganymede, Callisto
         1.370d0, 4.518d0,15.945d0,79.330d0,&! Saturn: Enceladus, Rhea, Titan, Iapetus
         1.413d0, 2.520d0,4.144d0,8.706d0, & ! Uranus: Miranda, Ariel, Umbriel, Titania
         5.877d0, 0.0d0,  0.0d0,  0.0d0], &  ! Neptune: Triton
        [4, 9])

end module orbital_data


module orbital_mechanics
    use iso_c_binding
    use orbital_data
    implicit none

contains

    ! Returns the (x, y) position of a body, in AU, at a given simulated
    ! time (in days since epoch). The Sun is fixed at the origin.
    subroutine get_position(body_index, time_days, x_au, y_au) &
            bind(c, name="get_position")
        integer(c_int), value :: body_index
        real(c_double), value :: time_days
        real(c_double), intent(out) :: x_au, y_au

        real(8) :: angle

        if (body_index < 1 .or. body_index > n_bodies) then
            x_au = 0.0d0
            y_au = 0.0d0
            return
        end if

        if (body_index == 1) then
            x_au = 0.0d0
            y_au = 0.0d0
            return
        end if

        angle = 2.0d0 * pi * time_days / orbital_period_days(body_index)
        x_au = semi_major_axis_au(body_index) * cos(angle)
        y_au = semi_major_axis_au(body_index) * sin(angle)
    end subroutine get_position

    ! Total number of simulated bodies (Sun + planets)
    function get_body_count() bind(c, name="get_body_count") result(n)
        integer(c_int) :: n
        n = n_bodies
    end function get_body_count

    ! Real radius of a body, in kilometers (for proportional rendering)
    function get_body_radius_km(body_index) &
            bind(c, name="get_body_radius_km") result(r)
        integer(c_int), value :: body_index
        real(c_double) :: r

        if (body_index < 1 .or. body_index > n_bodies) then
            r = 0.0d0
        else
            r = body_radius_km(body_index)
        end if
    end function get_body_radius_km

    ! Orbital period of a body, in Earth days (used for revolution counters)
    function get_orbital_period_days(body_index) &
            bind(c, name="get_orbital_period_days") result(p)
        integer(c_int), value :: body_index
        real(c_double) :: p

        if (body_index < 1 .or. body_index > n_bodies) then
            p = 0.0d0
        else
            p = orbital_period_days(body_index)
        end if
    end function get_orbital_period_days

    ! Number of simulated moons for a given body (0 if none)
    function get_moon_count(body_index) bind(c, name="get_moon_count") result(n)
        integer(c_int), value :: body_index
        integer(c_int) :: n

        if (body_index < 1 .or. body_index > n_bodies) then
            n = 0
        else
            n = moon_count(body_index)
        end if
    end function get_moon_count

    ! Unit-circle position (x, y both in [-1, 1]) of a moon relative to its
    ! parent body, at a given simulated time. JS scales and offsets this by
    ! the planet's screen position and a display radius of its own choosing,
    ! since real moon-planet distances are too small to render to true scale.
    subroutine get_moon_position_unit(body_index, moon_index, time_days, x_unit, y_unit) &
            bind(c, name="get_moon_position_unit")
        integer(c_int), value :: body_index, moon_index
        real(c_double), value :: time_days
        real(c_double), intent(out) :: x_unit, y_unit

        real(8) :: period, angle

        if (body_index < 1 .or. body_index > n_bodies .or. &
            moon_index < 1 .or. moon_index > max_moons) then
            x_unit = 0.0d0
            y_unit = 0.0d0
            return
        end if

        period = moon_period_days(moon_index, body_index)
        if (period <= 0.0d0) then
            x_unit = 0.0d0
            y_unit = 0.0d0
            return
        end if

        angle = 2.0d0 * pi * time_days / period
        x_unit = cos(angle)
        y_unit = sin(angle)
    end subroutine get_moon_position_unit

end module orbital_mechanics