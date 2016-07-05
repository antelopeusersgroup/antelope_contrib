! ----------------------------------------
subroutine mean(x, npts, avg)
  implicit none
  integer :: npts
  double precision, intent(in) :: x(npts)
  double precision, intent(out) :: avg
  avg = sum(x)/npts
end subroutine mean

! ----------------------------------------
subroutine std(x, npts, stdev)
  implicit none
  integer :: npts
  double precision, intent(in) :: x(npts)
  double precision :: stdev, avg
  call mean(x, npts, avg)
  stdev = sqrt(sum((x-avg)**2)/npts)
end subroutine std

! ----------------------------------------
subroutine matchfilter(template, nx, trace, ny, xcf)
    ! Matched filter algorithm
    ! Template matching against a longer trace
    implicit none
    integer :: nx, ny
    double precision, intent(in) :: template(nx), trace(ny)
    double precision, intent(out) :: xcf(ny+1-nx)
    double precision :: temp_arr(nx), mean, template0(nx)
    integer istart, iend
    istart = 1
    iend = nx
    template0 = template - sum(template)/nx
    do while (iend <= ny)
        if (istart == 1) then
            mean = sum(trace(istart:iend))
        else
            mean = mean + trace(iend) - trace(istart-1)
        end if
        temp_arr = trace(istart:iend) - mean/nx
        xcf(istart) = dot_product(temp_arr, template0)
        istart = istart + 1
        iend = iend + 1
    end do
end subroutine matchfilter

! ----------------------------------------
subroutine ray_tracer(l_depth, nz, l_vel, evdp, ta, X, T)
    ! Analytic ray tracer with linear gradients between layers
    ! l_depth: array of depths to top of each layer
    ! l_vel: array of vels at top of each layer
    ! evdp: source depth
    ! ta: takeoff angle

    implicit none

    ! Argument declarations
    integer :: nz
    double precision, intent(in) :: l_depth(nz), l_vel(nz), evdp, ta
    double precision, intent(out) :: X, T

    ! Variable declarations
    integer :: i, idx, src
    double precision :: u0, p0, p, pi, temp, eta1, eta2, t1, t2, x1, x2
    double precision, allocatable :: layers(:), slopes(:), u(:)

    pi = 3.14159265359

    ! Check if the source is on a boundary
    idx = 0
    do i = 1, nz
        if (l_depth(i) == evdp) then
            src = i
            u0 = 1.0/l_vel(i)
            idx = 1
            allocate(layers(nz))
            allocate(u(nz))
            u(1:nz) = 1.0/l_vel(1:nz)
            layers(1:nz) = l_depth(1:nz)
            exit
        end if
    end do

    ! Otherwise, split layer containing source in two
    if (idx == 0) then
        allocate(layers(nz+1))
        allocate(u(nz+1))
        do i = 1, nz
            if (l_depth(i) > evdp) then
                idx = i - 1
                exit
            end if
        end do
        layers(1:idx) = l_depth(1:idx)
        layers(idx+2:nz+1) = l_depth(idx+1:nz)
        layers(idx+1) = evdp
        src = idx+1
        u(1:idx) = l_vel(1:idx)
        u(idx+2:nz+1) = l_vel(idx+1:nz)
        temp = (l_vel(idx+1)-l_vel(idx))/(l_depth(idx+1)-l_depth(idx))
        u(idx+1) = l_vel(idx) + temp*(evdp-l_depth(idx))

        u0 = 1.0/u(idx+1)
        nz = nz + 1
    end if

    ! Calculate slopes between each boundary
    allocate(slopes(nz-1))
    do i = 1, nz-1
        slopes(i) = (u(i+1)-u(i))/(layers(i+1)-layers(i))
    end do

    ! Convert to slowness
    u = 1.0/u

    X = 0
    T = 0
    p0 = u0*sin(ta*pi/180.0)
    p = abs(p0)

    ! Branch for down-going rays
    if (p0 < 0) then
        ! Check that the ray will turn above the lowest layer
        if (u(nz) > p) then
            X = -999
            T = -999
            return
        end if

        ! Find the layer where the ray turns
        do i = 1, nz
            if (u(i) <= p) then
                idx = i
                exit
            end if
        end do

        ! Calculate travel times and ranges
        do i = src, idx-1
            eta1 = sqrt(u(i)**2 - p**2)
            x1 = eta1/(slopes(i)*u(i)*p)
            t1 = (log((u(i)+eta1)/p) - eta1/u(i)) / slopes(i)
            ! Break when in the turning layer
            if (i == idx-1) then
                X = X + 2*x1
                T = T + 2*(t1 + p*x1)
                exit
            end if
            eta2 = sqrt(u(i+1)**2 - p**2)
            x2 = eta2/(slopes(i)*u(i+1)*p)
            t2 = (log((u(i+1)+eta2)/p) - eta2/u(i+1)) / slopes(i)
            X = X + 2*(x1 - x2)
            T = T + 2*(t1 - t2 + p*(x1 - x2))
        end do
    end if

    ! Now perform calculations for up-going rays
    if (src == 0) then
        return
    end if

    do i = src, 2, -1
        eta1 = sqrt(u(i)**2 - p**2)
        eta2 = sqrt(u(i-1)**2 - p**2)
        ! Checking that ray is not perfectly vertical
        if (p /= 0) then
            x1 = eta1/(-1*slopes(i-1)*u(i)*p)
            x2 = eta2/(-1*slopes(i-1)*u(i-1)*p)
        else
            x1 = 0
            x2 = 0
        end if
        t1 = (log(u(i)+eta1) - eta1/u(i)) / (-1*slopes(i-1))
        t2 = (log(u(i-1)+eta2) - eta2/u(i-1)) / (-1*slopes(i-1))
        X = X + x1 - x2
        T = T + t1 - t2 + p*(x1 - x2)
    end do

    deallocate(layers)
    deallocate(u)
    deallocate(slopes)
    return

end subroutine ray_tracer

! ----------------------------------------
subroutine get_tt(dist, evdp, X, T, l_depth, l_vel, nz, tol)
    implicit none

    ! Argument declarations
    integer :: nz
    double precision, intent(in) :: l_depth(nz), l_vel(nz), tol, dist, evdp
    double precision, intent(out) :: X, T

    ! Variable declarations
    double precision :: XX, TT, dx, i_min, i_max, i

    ! Set up initial conditions for binary search
    X = 0.0
    T = 0.0
    i = 90.0
    call ray_tracer(l_depth, nz, l_vel, evdp, i, XX, TT)
    dx = XX - dist

    if (dx > 0) then
        i_min = 0.0
        i_max = 90.0
        ! Run binary search on up-going rays
        do while (abs(dx) >= tol)
            i = (i_max + i_min)/2.0
            call ray_tracer(l_depth, nz, l_vel, evdp, i, XX, TT)
            X = XX
            T = TT
            dx = XX - dist
            if (dx < 0) then
                i_min = i
            else
                i_max = i
            end if
        end do
    else
        i_min = -90.0
        i_max = 0.0
        ! Run binary search on down-going rays
        do while (abs(dx) >= tol)
            i = (i_max + i_min)/2.0
            call ray_tracer(l_depth, nz, l_vel, evdp, i, XX, TT)
            if (xx == -999) then
                i_max = i
                continue
            end if
            X = XX
            T = TT
            dx = XX - dist
            if (dx < 0) then
                i_min = i
            else
                i_max = i
            end if
        end do
    end if

    return

end subroutine get_tt

! ----------------------------------------
subroutine stalta(trace, cft, nsta, nlta, npts)

    ! recursive mean calculation
    implicit none

    ! Argument declarations
    integer :: npts
    double precision, intent(in) :: trace(npts), nsta, nlta
    double precision, intent(out) :: cft(npts)

    ! Variable declarations
    double precision :: alpha, beta, inv_alpha, inv_beta, square
    double precision :: sta, lta
    integer :: i

    alpha = 1/nsta
    beta = 1/nlta
    inv_alpha = 1 - alpha
    inv_beta = 1 - beta
    sta = 0
    lta = 1e-30

    do i = 2, npts
        square = trace(i)**2
        sta = alpha*square + inv_alpha*sta
        lta = beta*square + inv_beta*lta
        cft(i) = sta/lta
        if (i <= nlta) then
            cft(i) = 0
        end if
    end do

end subroutine stalta

! ----------------------------------------
subroutine snr(trace, cft, nsta, nlta, npts, on, off)

    ! recursive mean calculation
    implicit none

    ! Argument declarations
    integer :: npts
    double precision, intent(in) :: trace(npts), on, off
    integer, intent(in) :: nsta, nlta
    double precision, intent(out) :: cft(npts)

    ! Variable declarations
    double precision :: sta, lta, X(nsta), Y(nlta)
    integer :: state, i
    state = 0
    cft(1:nlta) = 0
    do i = nlta, npts
        X = trace(i-nsta+1:i)
        Y = trace(i-nlta+1:i)
        sta = sum(X**2)/nsta
        if (state == 0) then
            lta = sum(Y**2)/nlta
            cft(i) = sta/lta
            if (cft(i) >= on) then
                state = 1
            end if
        else
            cft(i) = sta/lta
            if (cft(i) < off) then
                state = 0
            end if
        end if
    end do

end subroutine snr

! ----------------------------------------
subroutine pai_s(trace, output, M, npts)
  ! PAI-S Skewness function defined by Saragiotis et al. (2002)
  implicit none

  ! Argument declarations
  integer :: npts, M
  double precision, intent(in) :: trace(npts)
  double precision, intent(out) :: output(npts)

  ! Variable declarations
  double precision :: numer, denom, X(M), mean, diff(M)
  integer :: i

  output = 0
  mean = sum(trace(1:M))
  do i = M, npts
    X = trace(i-M+1:i)
    mean = mean - trace(i-M+1) + trace(i+1)
    diff = (X-mean/M)
    numer = sum(diff**3)/M
    denom = (sum(diff**2)/(M-1))**(3./2.)
    output(i) = numer/denom
  end do

end subroutine PAI_S

! ----------------------------------------
subroutine pai_k(trace, output, M, npts)
  ! PAI-K Kurtosis subroutine defined by Saragiotis et al. (2002)
  implicit none

  ! Argument declarations
  integer :: npts, M
  double precision, intent(in) :: trace(npts)
  double precision, intent(out) :: output(npts)

  ! Variable declarations
  double precision :: numer, denom, X(M), mean, diff(M)
  integer :: i

  output = 0
  mean = sum(trace(1:M))
  do i = M, npts
    X = trace(i-M+1:i)
    mean = mean - trace(i-M+1) + trace(i+1)
    diff = (X-mean/M)**2
    denom = (sum(diff)/M)**2
    numer = sum(diff**2)/M
    output(i) = numer/denom - 3.0
    if (isnan(output(i))) output(i) = 0
  end do

end subroutine PAI_K

! ----------------------------------------
subroutine cov_filter(Z, N, E, r, phi, npts, wlen)
    ! Recursive sliding covariance matrix calculation
    implicit none

    ! Argument declarations
    double precision, intent(in) :: Z(npts), E(npts), N(npts)
    double precision, intent(out) :: r(npts), phi(npts)
    double precision :: C(3,3), W(3), work(8)
    double precision :: CNN(npts), CEE(npts), CZZ(npts)
    double precision :: CNE(npts), CNZ(npts), CZE(npts)
    integer :: npts, info, i, wlen, lwork

    ! Initialize variables & matrices
    r(1:npts) = 0
    phi(1:npts) = 1
    lwork = 8

    r(1:wlen) = 0
    phi(1:wlen) = 0

    call moving_cov(N, N, CNN, npts, wlen)
    call moving_cov(E, E, CEE, npts, wlen)
    call moving_cov(Z, Z, CZZ, npts, wlen)
    call moving_cov(N, Z, CNZ, npts, wlen)
    call moving_cov(N, E, CNE, npts, wlen)
    call moving_cov(Z, E, CZE, npts, wlen)

    do i = wlen+1, npts
        ! Store result in matrix for given time index
        C(1,1) = CEE(i)
        C(2,2) = CNN(i)
        C(3,3) = CZZ(i)
        C(1,2) = CNE(i)
        C(1,3) = CZE(i)
        C(2,3) = CNZ(i)
        C(3,2) = C(2,3)
        C(3,1) = C(1,3)
        C(2,1) = C(1,2)

        ! Calculate eigenvectors
        call DSYEV('V', 'U', 3, C, 3, W, work, lwork, info)

        ! Store polarization quantities
        r(i) = 1 - (W(1)+W(2))/(2*W(3))
        phi(i) = abs(C(3,3))
    end do

end subroutine cov_filter

! ----------------------------------------
subroutine cov_filter2(Z, N, E, r, phi, npts, wlen)
  ! Constructs a polarization filter from 3-component data
  implicit none

  ! Argument declarations
  double precision, intent(in) :: Z(npts), E(npts), N(npts)
  double precision, intent(out) :: r(npts), phi(npts)
  double precision :: A(3,3), W(3), work(8)
  double precision :: X(wlen, 3)
  integer :: npts, info, i, wlen, lwork

  ! Initialize variables & matrices
  r(1:npts) = 0
  phi(1:npts) = 1
  lwork = 8

  do i = 1, npts - wlen
    ! Window traces and calculate COV matrix
    X(:,1) = E(i:i+wlen) - sum(E(i:i+wlen))/wlen
    X(:,2) = N(i:i+wlen) - sum(N(i:i+wlen))/wlen
    X(:,3) = Z(i:i+wlen) - sum(Z(i:i+wlen))/wlen
    A = matmul(transpose(X), X)

    ! Calculate eigenvectors
    call DSYEV('V', 'U', 3, A, 3, W, work, lwork, info)

    if (i+wlen <= npts) then
      r(i+wlen) = 1 - (W(1)+W(2))/(2*W(3))
      phi(i+wlen) = abs(A(3,3))
    end if

  end do

end subroutine cov_filter2

! ----------------------------------------
subroutine moving_cov(X, Y, C, npts, wlen)
    implicit none
    double precision, intent(in) :: X(npts), Y(npts)
    double precision, intent(out) :: C(npts)
    double precision :: xm, ym, xm_old, ym_old, cov
    integer, intent(in) :: wlen
    integer :: npts, den, i

    xm = sum(X(1:wlen))
    ym = sum(Y(1:wlen))
    cov = dot_product(X(1:wlen)-xm/wlen, Y(1:wlen)-ym/wlen)
    den = npts*npts
    C(1:wlen) = 0
    do i = wlen+1, npts
        xm_old = xm
        ym_old = ym
        xm = xm + X(i) - X(i-wlen)
        ym = ym + Y(i) - Y(i-wlen)
        cov = cov + X(i)*Y(i) - X(i-wlen)*Y(i-wlen) !+ npts*(xm_old*ym_old/den -xm*ym/den) 
        C(i) = cov
    end do

end subroutine moving_cov

! ----------------------------------------
subroutine trigger(x, t_on, t_off, N, ntrig, on, off)
  implicit none
  double precision, intent(in) :: x(N), on, off
  integer, intent(out) :: t_on(N), t_off(N), ntrig
  integer, intent(in) :: N
  integer :: i, mode

  mode = 0
  ntrig = 1

  do i = 1, N
    if (mode == 0) then
      if (x(i) >= on) then
        t_on(ntrig) = i - 1
        mode = 1
      end if
    else
      if (x(i) <= off) then
        t_off(ntrig) = i - 1
        ntrig = ntrig + 1
        mode = 0
      end if
    end if
  end do
  if (mode == 1) then
    t_off(ntrig) = N - 1
  end if

end subroutine trigger

! ----------------------------------------
subroutine migrate_1d(D, TT, nwin, nsta, nsamp, npts, M, X, Y, Z, nx, ny, nz, ctr, wght_on)
    ! D is an array of traces
    ! TT is a 4D array of travel times (in samples)
    ! nwin is the half-width of the stacking window in samples
    implicit none

    ! Argument declarations
    integer nx, ny, nz, nsta, nsamp, npts, wght_on
    double precision, intent(in)  :: D(nsta,nsamp)
    double precision, intent(out) :: M(npts)
    integer, intent(in)           :: TT(nsta,nx,ny,nz), nwin
    integer, intent(out)          :: X(npts), Y(npts), Z(npts), ctr

    ! Variable declarations
    integer :: i, j, k, l, n, ix, iy, iz, istart, iend, ia, ib
    double precision :: peak, tmp, weight(nwin*2)

    M = 0.0
    ctr = 0
    do i = 1, nwin*2
        weight(i) = dcos((i-1)*180d0/(nwin*2 - 1))
    end do
    weight = weight / sum(weight)

    !For each time step, calculate migration index
    outer: do n = 1, npts, nwin
        ix = 0
        iy = 0
        iz = 0
        peak = 0d0
        ia = n - nwin
        ib = n + nwin

        do i = 1, nx
            do j = 1, ny
                do k = 1, nz
                    tmp = 0d0
                    do l = 1, nsta
                        istart = ia + TT(l,i,j,k)
                        if (istart <= 0) then
                            M(ctr+1) = 0
                            X(ctr+1) = 1
                            Y(ctr+1) = 1
                            Z(ctr+1) = 1
                            ctr = ctr + 1
                            cycle outer
                        end if
                        iend = ib + TT(l,i,j,k)
                        if (iend > nsamp) return
                        if (wght_on == 1) then
                            tmp = tmp + sum(D(l,istart:iend)*weight)
                        else if (wght_on == 0) then
                            tmp = tmp + sum(D(l,istart:iend))
                        end if
                    end do
                    if (tmp > peak) then
                        peak = tmp
                        ix = i
                        iy = j
                        iz = k
                    end if
                end do
            end do
        end do
        M(ctr+1) = peak
        X(ctr+1) = ix - 1
        Y(ctr+1) = iy - 1
        Z(ctr+1) = iz - 1
        ctr = ctr + 1
    end do outer

end subroutine migrate_1d

! ----------------------------------------
subroutine migrate_pso(D, Xr, Yr, Zr, Xmin, Xmax, Ymin, Ymax, &
  & Zmin, Zmax, np, c1, c2, nsta, nsamp, npts, M, X, Y, Z, dt, vel)
    implicit none

    ! Argument declarations
    integer :: nsta, nsamp, npts, np
    double precision, intent(in)  :: c1, c2, dt, vel
    double precision, intent(in)  :: D(nsta,nsamp), Xmin, Xmax, Ymin, Ymax
    double precision, intent(in)  :: Zmin, Zmax, Xr(nsta), Yr(nsta), Zr(nsta)
    double precision, intent(out) :: M(npts), X(npts), Y(npts), Z(npts)

    ! Variable declarations
    integer :: n
    double precision :: gbest(3)

    M = 0.0
    ! For each time step, calculate migration index
    do n = 1, npts
        call swarm(M(n), n, Xr, Yr, Zr, D, nsta, nsamp, Xmin, Xmax, Ymin, &
                    Ymax, Zmin, Zmax, np, c1, c2, dt, gbest, vel)
        X(n) = gbest(1)
        Y(n) = gbest(2)
        Z(n) = gbest(3)
    end do

end subroutine migrate_pso

! ----------------------------------------
subroutine swarm(M, n, Xr, Yr, Zr, D, nsta, nsamp, &
  & Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, np, c1, c2, dt, gbest, vel)
    ! particle swarm optimization algorithm
    ! M: double for Migration index output
    ! i[xyz]: index of best [XYZ]
    ! n: Present value of time series
    ! [XYZ]r: Receiver locations relative to reference
    ! D: Data matrix (nsta, nsamp)
    ! nsta: number of stations
    ! nsamp: number of samples for data traces
    ! [XYZ][min,max]: Range of values for parameter search
    ! np: number of particles per swarm
    ! c1: learning factor
    ! c2: other learning factor
    ! dt: sampling interval
    implicit none

    ! Argument declarations
    integer nsta, nsamp, np
    double precision, intent(in) :: Xr(nsta), Yr(nsta), Zr(nsta)
    double precision, intent(in) :: D(nsta,nsamp), dt, c1, c2, vel
    double precision, intent(in) :: Xmin, Xmax, Ymin, Ymax, Zmin, Zmax
    integer, intent(in) :: n
    double precision, intent(out) :: M, gbest(3)

    ! Variable declarations
    integer i, g
    double precision :: fpbest(np), pbest(np,3), fgbest
    double precision :: x(np), y(np), z(np)
    double precision :: xrange, yrange, zrange, f, tmp1, tmp2, tmp3
    double precision :: v(np,3), rp, rg, vmax(3), temp

    call random_seed()
    x = 0d0
    y = 0d0
    z = 0d0
    xrange = Xmax - Xmin
    yrange = Ymax - Ymin
    zrange = Zmax - Zmin
    fpbest = 0
    fgbest = 0
    gbest = 0
    pbest = 0
    vmax(1) = xrange
    vmax(2) = yrange
    vmax(3) = zrange

    ! Initialize particles, velocities, locations
    do i = 1, np
        call random_number(x(i))
        call random_number(y(i))
        call random_number(z(i))
        call random_number(temp)
        x(i) = x(i)*xrange + Xmin
        y(i) = y(i)*yrange + Ymin
        z(i) = z(i)*zrange + Zmin
        v(i,1) = 2*temp*abs(xrange)
        v(i,2) = 2*temp*abs(yrange)
        v(i,3) = 2*temp*abs(zrange)
        if (v(i,1) > vmax(1)) then
            do while (v(i,1) > vmax(1))
                call random_number(temp)
                v(i,1) = 2*temp*vmax(1)
                v(i,2) = 2*temp*vmax(2)
                v(i,3) = 2*temp*vmax(3)
            end do
        end if
    end do

    do g = 1, 20
        ! Calculate fitness for each particle and update bests
        do i = 1, np
            call obj(x(i), y(i), z(i), Xr, Yr, Zr, nsta, D, nsamp, n, f, dt, vel)

            ! Calculate and possibly update personal best
            if (f > fpbest(i)) then
                fpbest(i) = f
                pbest(i,1) = x(i)
                pbest(i,2) = y(i)
                pbest(i,3) = z(i)
            end if

            ! Calculate and possibly update group best
            if (f > fgbest) then
                fgbest = f
                gbest(1) = x(i)
                gbest(2) = y(i)
                gbest(3) = z(i)
            end if

        end do

        ! Calculate new velocity and update particle location
        do i = 1, np
            call random_number(rp)
            call random_number(rg)
            tmp1 = v(i,1) + c1*rp*(pbest(i,1)-x(i)) + c2*rg*(gbest(1)-x(i))
            tmp2 = v(i,2) + c1*rp*(pbest(i,2)-y(i)) + c2*rg*(gbest(2)-y(i))
            tmp3 = v(i,3) + c1*rp*(pbest(i,3)-z(i)) + c2*rg*(gbest(3)-z(i))

            ! Checking velocity bounds on each parameter
            if ((tmp1 > vmax(1)).or.(tmp1 < -vmax(1))) then
                do while ((tmp1 > vmax(1)).or.(tmp1 < -vmax(1)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp1 = v(i,1) + c1*rp*(pbest(i,1)-x(i)) + c2*rg*(gbest(1)-x(i))
                end do
            end if
            v(i,1) = tmp1
            if ((tmp2 > vmax(2)).or.(tmp2 < -vmax(2))) then
                do while ((tmp2 > vmax(2)).or.(tmp2 < -vmax(2)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp2 = v(i,2) + c1*rp*(pbest(i,2)-y(i)) + c2*rg*(gbest(2)-y(i))
                end do
            end if
            v(i,2) = tmp2
            if ((tmp3 > vmax(3)).or.(tmp3 < -vmax(3))) then
                do while ((tmp3 > vmax(3)).or.(tmp3 < -vmax(3)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp3 = v(i,3) + c1*rp*(pbest(i,3)-z(i)) + c2*rg*(gbest(3)-z(i))
                end do
            end if
            v(i,3) = tmp3

            ! Update particle location
            x(i) = x(i) + v(i,1)
            y(i) = y(i) + v(i,2)
            z(i) = z(i) + v(i,3)

            ! Ensure new location is within bounds
            if (x(i) > Xmax) then
                x(i) = Xmax
            else if (x(i) < Xmin) then
                x(i) = Xmin
            end if
            if (y(i) > Ymax) then
                y(i) = Ymax
            else if (y(i) < Ymin) then
                y(i) = Ymin
            end if
            if (z(i) > Zmax) then
                z(i) = Zmax
            else if (Z(i) < Zmin) then
                z(i) = Zmin
            end if
        end do
    end do
    M = fgbest

end subroutine swarm

! ----------------------------------------
subroutine obj(x, y, z, Xr, Yr, Zr, nsta, D, nsamp, n, f, dt, vel)
    implicit none

    ! Argument declarations
    integer :: nsta, nsamp, n
    double precision, intent(in) :: x, y, z, Xr(nsta), Yr(nsta), Zr(nsta)
    double precision, intent(in) :: D(nsta,nsamp), dt, vel
    double precision, intent(out) :: f

    ! Variable declarations
    integer :: i, idx
    double precision :: t

    f = 0.0
    do i = 1, nsta
        t = sqrt((Xr(i)-x)**2 + (Yr(i)-y)**2 + (Zr(i)+z)**2)/vel
        idx = int(t/dt) + n
        f = f + D(i,idx)
    end do

end subroutine obj

! ----------------------------------------
subroutine Tpd(tr, nx, T, dt, tw, tmx, noise_dur)
    ! tr: numpy array with nx samples
    ! dt: sampling interval
    ! tw: T_w as defined in Hildyard et al. (2008)
    ! tmx: T_mx as defined in above
    ! noise_dur: The duration over which to calculate noise amplitude
    implicit none

    ! Argument declarations
    integer :: nx
    double precision, intent(in) :: tr(nx), dt, tw, tmx, noise_dur
    double precision, intent(out) :: T(nx)

    ! Variable declarations
    integer :: nn, i
    double precision :: Ds, alpha, noise, pi, twopi
    double precision :: X(nx), D(nx)

    pi = 3.14159265359
    alpha = exp(log(0.1)/(tw/dt))
    twopi = pi*2

    ! Calculate noise level
    nn = int(noise_dur/dt)
    noise = sum(tr(1:nn)**2)/nn
    Ds = twopi**2 * noise * (tw/dt) / (tmx**2)
    X(1) = tr(1)**2
    D(1) = 0
    T(1) = 0

    ! Calculate X
    do i = 2, nx
        X(i) = alpha*X(i-1) + tr(i)**2
        D(i) = alpha*D(i-1) + ((tr(i)-tr(i-1))/dt)**2
        T(i) = twopi * sqrt(X(i)/(D(i) + Ds))
    end do

end subroutine Tpd

! ----------------------------------------
function to_radian(degree) result(rad)
    ! degrees to radians
    real,intent(in) :: degree
    real :: rad,pi

    pi = 4*atan(1.0)  ! exploit intrinsic atan to generate pi
    rad = degree*pi/180
end function to_radian

! ----------------------------------------
function haversine(deglat1,deglon1,deglat2,deglon2) result (dist)
    ! great circle distance -- adapted from Matlab
    real,intent(in) :: deglat1,deglon1,deglat2,deglon2
    real :: a,c,dist,dlat,dlon,lat1,lat2
    real,parameter :: radius = 6372.8

    dlat = to_radian(deglat2-deglat1)
    dlon = to_radian(deglon2-deglon1)
    lat1 = to_radian(deglat1)
    lat2 = to_radian(deglat2)
    a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
    c = 2*asin(sqrt(a))
    dist = radius*c
end function haversine

! ----------------------------------------
subroutine searchsorted(x, nx, s, i)
    implicit none
    integer nx
    double precision, intent(in) :: x(nx), s
    integer, intent(out) :: i

    integer :: mid, high, low
    low = 1
    high = nx

    do while (low <= high)
        mid = (high + low)/2
        if (x(mid) > s) then
            if ((mid-low) == 1) then
                i = low
                return
            end if
            high = mid
        else if (x(mid) < s) then
            if ((high-mid) == 1) then
                i = mid
                return
            end if
            low = mid
        else
            i = mid
            return
        end if
    end do

end subroutine searchsorted

! ----------------------------------------
subroutine interp(x, y, nx, s, ns, r)
    implicit none
    integer nx, ns
    double precision, intent(in) :: x(nx), y(nx), s(ns)
    double precision, intent(out) :: r(ns)
    integer :: i, idx

    do i = 1, ns
        call searchsorted(x, nx, s(i), idx)
        r(i) = y(idx) + (y(idx+1)-y(idx))*((s(i)-x(idx))/(x(idx+1)-x(idx)))
    end do

end subroutine interp


! ----------------------------------------
subroutine interp2d(x, y, f, nx, ny, x0, y0, n0, r)
    implicit none

    ! Argument declarations
    integer nx, ny, n0
    double precision, intent(in) :: x(nx), y(ny), f(nx,ny), x0(n0), y0(n0)
    double precision, intent(out) :: r(n0)

    ! Variable declarations
    integer :: i, ix, iy, x2, x1, y2, y1
    double precision :: Q11, Q12, Q21, Q22

    do i = 1, n0
        call searchsorted(x, nx, x0(i), ix)
        call searchsorted(y, ny, y0(i), iy)
        x1 = ix
        x2 = ix + 1
        y1 = iy
        y2 = iy + 1
        Q11 = f(x1,y1)
        Q22 = f(x2,y2)
        Q12 = f(x1,y2)
        Q21 = f(x2,y1)
        r(i) = 1/((x(x2)-x(x1))*(y(y2)-y(y1))) * (&
               Q11*(x(x2)-x0(i))*(y(y2)-y0(i)) + &
               Q21*(x0(i)-x(x1))*(y(y2)-y0(i)) + &
               Q12*(x(x2)-x0(i))*(y0(i)-y(y1)) + &
               Q22*(x0(i)-x(x1))*(y0(i)-y(y1))   )
    end do

end subroutine interp2d

! ----------------------------------------
subroutine mfp(D, XX, TT, Xs, Ys, Xr, Yr, freqs, nf, nsta, ntt, B, nx, ny, nz, mode)
    implicit none

    ! Argument declarations
    integer nx, ny, nz, nsta, nf, ntt, mode
    double precision, intent(in)  :: Xs(nx), Ys(ny)
    double precision, intent(in)  :: Xr(nsta), Yr(nsta), freqs(nf)
    double precision, intent(in)  :: TT(ntt,nz), XX(ntt,nz)
    double precision, intent(out) :: B(nx,ny,nz)
    complex*16, intent(in)         :: D(nsta,nf)

    ! Variable declarations
    integer :: i, j, k, f, IPIV(nsta), info
    complex*16 :: KK(nsta,nsta,nf), norm, rep(nsta), WORK(nsta), const
    double precision :: a(nsta), dt(nsta), pi

    pi = 3.14159265359d0
    B = 0.0
    const = dcmplx(0.0, -2.*pi)

    !For each frequency, calculate the Bartlett operator
    do f = 1, nf
        ! Calculate CSDM
        do i = 1, nsta
        do j = 1, nsta
                KK(i,j,f) = D(i,f)*conjg(D(j,f))
        end do
        end do

        norm = 0
        do i = 1, nsta
        do j = 1, nsta
            norm = KK(i,j,f)*conjg(KK(i,j,f)) + norm
        end do
        end do
        KK(:,:,f) = KK(:,:,f)/sqrt(norm)

        if (mode == 1) then
            call ZGETRF(nsta, nsta, KK(:,:,f), nsta, IPIV, WORK, nsta, info)
            if (info .ne. 0) then
                write (*,*) "failed to get LU factorization"
                return
            end if
            call ZGETRI(nsta, KK(:,:,f), nsta, IPIV, WORK, nsta, info)
            if (info .ne. 0) then
                write (*,*) "failed to get inverse of CSDM"
                return
            end if
        end if
    end do

    do i = 1, nx
        do j = 1, ny
            a = sqrt((Xr-Xs(i))**2 + (Yr-Ys(j))**2)
            do k = 1, nz
                ! Interpolate travel times and construct replicas
                call interp(XX(:,k), TT(:,k), ntt, a, nsta, dt)
                rep = const*dt

                ! Calculate bartlett processor
                if (mode == 0) call bartlett(rep, KK, nsta, freqs, nf, B(i,j,k))
                if (mode == 1) call mvdr(rep, KK, nsta, freqs, nf, B(i,j,k))
            end do
        end do
    end do
    B = B/nf

end subroutine mfp

! ----------------------------------------
subroutine bartlett(rep, K, nsta, freqs, nf, B)
    implicit none

    ! Argument declarations
    integer               :: nsta, nf
    complex*16, intent(in) :: rep(nsta), K(nsta,nsta,nf)
    double precision, intent(in) :: freqs(nf)
    double precision, intent(out) :: B

    ! Variable declarations
    integer               :: i, j, f
    complex*16            :: temp(nsta), t_rep(nsta), norm

    B = 0
    do f = 1, nf
        t_rep = exp(rep*freqs(f))
        norm = sqrt(sum(t_rep*conjg(t_rep)))
        t_rep = t_rep/sqrt(sum(t_rep*conjg(t_rep)))

        temp = 0
        do i = 1, nsta
            do j = 1, nsta
                temp(i) = temp(i) + K(i,j,f)*t_rep(j)
            end do
        end do

        B = B + abs(sum(conjg(t_rep)*temp))
    end do

end subroutine bartlett

! ----------------------------------------
subroutine mvdr(rep, K, nsta, freqs, nf, B)
    implicit none

    ! Argument declarations
    integer               :: nsta, nf
    complex*16, intent(in) :: rep(nsta), K(nsta,nsta,nf)
    double precision, intent(in) :: freqs(nf)
    double precision, intent(out) :: B

    ! Variable declarations
    integer               :: i, j, f
    complex*16            :: temp(nsta), t_rep(nsta), norm

    B = 0
    do f = 1, nf
        t_rep = exp(rep*freqs(f))
        norm = sqrt(sum(t_rep*conjg(t_rep)))
        t_rep = t_rep/sqrt(sum(t_rep*conjg(t_rep)))

        temp = 0
        do i = 1, nsta
            do j = 1, nsta
                temp(i) = temp(i) + K(i,j,f)*t_rep(j)
            end do
        end do

        B = B + 1d0/abs(sum(conjg(t_rep)*temp))
    end do

end subroutine mvdr

! ----------------------------------------
subroutine mfp_pso(D, XX, ZZ, TT, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Xr, Yr, &
                   freqs, nf, nsta, ntt, nz, gbest, fgbest, np)
    implicit none

    ! Argument declarations
    integer nsta, np, nf, ntt, nz
    double precision, intent(in)  :: TT(ntt,nz), XX(ntt), ZZ(nz)
    double precision, intent(in) :: Xr(nsta), Yr(nsta), freqs(nf)
    double precision, intent(in) :: Xmin, Xmax, Ymin, Ymax, Zmin, Zmax
    complex*16, intent(in)         :: D(nsta,nf)
    double precision, intent(out) :: gbest(3), fgbest

    ! Variable declarations
    integer i, j, g, fr
    double precision :: fpbest(np), pbest(np,3)
    double precision :: x(np), y(np), z(np), B
    double precision :: xrange, yrange, zrange, f, tmp1, tmp2, tmp3
    double precision :: v(np,3), rp, rg, vmax(3), temp, c1, c2
    complex*16 :: KK(nsta,nsta,nf), norm

    c1 = 2.
    c2 = 2.

    B = 0.0

    !For each frequency, calculate the Bartlett operator
    do fr = 1, nf
        ! Calculate CSDM
        do i = 1, nsta
        do j = 1, nsta
                KK(i,j,fr) = D(i,fr)*conjg(D(j,fr))
        end do
        end do

        norm = 0
        do i = 1, nsta
        do j = 1, nsta
            norm = KK(i,j,fr)*conjg(KK(i,j,fr)) + norm
        end do
        end do
        KK = KK/sqrt(norm)
    end do

    call random_seed()
    x = 0d0
    y = 0d0
    z = 0d0
    xrange = Xmax - Xmin
    yrange = Ymax - Ymin
    zrange = Zmax - Zmin
    fpbest = 0
    fgbest = 0
    gbest = 0
    pbest = 0
    vmax(1) = xrange
    vmax(2) = yrange
    vmax(3) = zrange

    ! Initialize particles, velocities, locations
    do i = 1, np
        call random_number(x(i))
        call random_number(y(i))
        call random_number(z(i))
        call random_number(temp)
        x(i) = x(i)*xrange + Xmin
        y(i) = y(i)*yrange + Ymin
        z(i) = z(i)*zrange + Zmin
        v(i,1) = 2*temp*abs(xrange)
        v(i,2) = 2*temp*abs(yrange)
        v(i,3) = 2*temp*abs(zrange)
        if (v(i,1) > vmax(1)) then
            do while (v(i,1) > vmax(1))
                call random_number(temp)
                v(i,1) = 2*temp*vmax(1)
                v(i,2) = 2*temp*vmax(2)
                v(i,3) = 2*temp*vmax(3)
            end do
        end if
    end do

    do g = 1, 20
        ! Calculate fitness for each particle and update bests
        do i = 1, np
            call obj_bartlett(x(i), y(i), z(i), Xr, Yr, KK, freqs, nf, nsta, &
                          XX, ZZ, TT, ntt, nz, f)

            ! Calculate and possibly update personal best
            if (f > fpbest(i)) then
                fpbest(i) = f
                pbest(i,1) = x(i)
                pbest(i,2) = y(i)
                pbest(i,3) = z(i)
            end if

            ! Calculate and possibly update group best
            if (f > fgbest) then
                fgbest = f 
                gbest(1) = x(i)
                gbest(2) = y(i)
                gbest(3) = z(i) 
            end if
       
        end do

        ! Calculate new velocity and update particle location
        do i = 1, np
            call random_number(rp)
            call random_number(rg)
            tmp1 = v(i,1) + c1*rp*(pbest(i,1)-x(i)) + c2*rg*(gbest(1)-x(i))
            tmp2 = v(i,2) + c1*rp*(pbest(i,2)-y(i)) + c2*rg*(gbest(2)-y(i))
            tmp3 = v(i,3) + c1*rp*(pbest(i,3)-z(i)) + c2*rg*(gbest(3)-z(i))

            ! Checking velocity bounds on each parameter
            if ((tmp1 > vmax(1)).or.(tmp1 < -vmax(1))) then
                do while ((tmp1 > vmax(1)).or.(tmp1 < -vmax(1)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp1 = v(i,1) + c1*rp*(pbest(i,1)-x(i)) + c2*rg*(gbest(1)-x(i))
                end do
            end if
            v(i,1) = tmp1
            if ((tmp2 > vmax(2)).or.(tmp2 < -vmax(2))) then
                do while ((tmp2 > vmax(2)).or.(tmp2 < -vmax(2)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp2 = v(i,2) + c1*rp*(pbest(i,2)-y(i)) + c2*rg*(gbest(2)-y(i))
                end do
            end if
            v(i,2) = tmp2
            if ((tmp3 > vmax(3)).or.(tmp3 < -vmax(3))) then
                do while ((tmp3 > vmax(3)).or.(tmp3 < -vmax(3)))
                    call random_number(rp)
                    call random_number(rg)
                    tmp3 = v(i,3) + c1*rp*(pbest(i,3)-z(i)) + c2*rg*(gbest(3)-z(i))
                end do
            end if
            v(i,3) = tmp3

            ! Update particle location
            x(i) = x(i) + v(i,1)
            y(i) = y(i) + v(i,2)
            z(i) = z(i) + v(i,3)

            ! Ensure new location is within bounds
            if (x(i) > Xmax) then
                x(i) = Xmax
            else if (x(i) < Xmin) then
                x(i) = Xmin
            end if
            if (y(i) > Ymax) then
                y(i) = Ymax
            else if (y(i) < Ymin) then
                y(i) = Ymin
            end if
            if (z(i) > Zmax) then
                z(i) = Zmax
            else if (Z(i) < Zmin) then
                z(i) = Zmin
            end if
        end do
    end do

end subroutine mfp_pso

subroutine obj_bartlett(x, y, z, Xr, Yr, KK, freqs, nf, nsta, XX, ZZ, TT, ntt, nz, B)
    implicit none

    ! Argument declarations
    integer nz, nsta, nf, ntt
    double precision, intent(in)  :: x, y, z
    double precision, intent(in)  :: Xr(nsta), Yr(nsta), freqs(nf)
    double precision, intent(in)  :: TT(ntt,nz), XX(ntt), ZZ(nz)
    double precision, intent(out) :: B
    complex*16, intent(in)         :: KK(nsta,nsta,nf)

    ! Variable declarations
    integer :: i, j, f
    complex*16 :: norm, rep(nsta), temp(nsta)
    double precision :: a(nsta), dt(nsta), pi, zzz(nsta)

    ! Interpolate travel times for all receivers
    B = 0
    pi = 3.14159265359
    zzz = z
    a = sqrt((Xr-x)**2 + (Yr-y)**2)
    do f = 1, nf
        call interp2d(XX, ZZ, TT, ntt, nz, a, zzz, nsta, dt)

        rep = exp(2*(0,-1)*pi*freqs(f)*dt)
        norm = 0
        do i = 1, nsta
            norm = norm + rep(i)*conjg(rep(i))
        end do
        rep = rep / sqrt(norm)

        temp = 0
        do i = 1, nsta
            do j = 1, nsta
                temp(i) = temp(i) + KK(i,j,f)*rep(j)
            end do
        end do
        do i = 1, nsta
            B = B + abs(conjg(rep(i))*temp(i))
        end do
    end do
    B = B/nf

end subroutine obj_bartlett
