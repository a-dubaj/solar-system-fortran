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

end module orbital_mechanics