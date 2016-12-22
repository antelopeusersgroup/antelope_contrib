subroutine refit_1fc(spec, freqs, fc, omega, n, oo, ff, nf, res)
    implicit none
    ! Argument declarations
    integer :: ff, oo, nf
    
    double precision, intent(in) :: omega(oo), fc(ff), n(ff), spec(nf), freqs(nf)
    double precision, intent(out) :: res(3)

    ! Variable declarations
    double precision :: synth(nf), best_ssq, ssq, weights(nf)
    integer :: i, k
    best_ssq = HUGE(ssq)
    weights = 1!/freqs
    do i = 1, ff
        do k = 1, oo
            synth = omega(k) / (1 + (freqs/fc(i))**n(i))
            ssq = sum(((synth-spec)*weights)**2)
            if (ssq < best_ssq) then
                best_ssq = ssq
                res(1) = fc(i)
                res(2) = omega(k)
                res(3) = n(i)
            end if
        end do
    end do
end subroutine refit_1fc

subroutine refit_2fc(spec, freqs, fc1, fc2, omega, n, oo, ff, gg, nf, res)
    implicit none
    ! Argument declarations
    integer :: ff, oo, nf, gg
    double precision, intent(in) :: omega(oo), n(ff), spec(nf), freqs(nf)
    double precision, intent(in) :: fc1(ff), fc2(gg)
    double precision, intent(out) :: res(4)

    ! Variable declarations
    double precision :: synth(nf), best_ssq, ssq, weights(nf)
    integer :: i, k, l
    best_ssq = HUGE(ssq)
    do i = 1, ff
        do k = 1, oo
            do l = 1, gg
                !synth = log10(omega(k)) + log10(1 + (freqs/fc2(l))**n(i))
                !synth = synth - log10(1 + (freqs/fc1(i))**n(i))
                synth = (1 + (freqs/fc2(l))**n(i))
                synth = synth / (1 + (freqs/fc1(i))**n(i))
                synth = synth * omega(k)
                ssq = sum((synth-spec)**2)
                if (ssq < best_ssq) then
                    best_ssq = ssq
                    res(1) = fc1(i)
                    res(2) = omega(k)
                    res(3) = n(i)
                    res(4) = fc2(l)
                end if
            end do
        end do
    end do

end subroutine refit_2fc

subroutine fit_1fc(spec, freqs, fc, omega, n, oo, nn, ff, nf, res)
    implicit none
    ! Argument declarations
    integer :: ff, nn, oo, nf
    double precision, intent(in) :: omega(oo), fc(ff), n(nn), spec(nf), freqs(nf)
    double precision, intent(out) :: res(3)

    ! Variable declarations
    double precision :: synth(nf), best_ssq, ssq, weights(nf)
    integer :: i, j, k
    best_ssq = HUGE(ssq)
    weights = 1/freqs
    do i = 1, ff
        do j = 1, nn
            do k = 1, oo
                synth = omega(k) / (1 + (freqs/fc(i))**n(j))
                ssq = sum(((synth-spec))**2)
                if (ssq < best_ssq) then
                    best_ssq = ssq
                    res(1) = fc(i)
                    res(2) = omega(k)
                    res(3) = n(j)
                end if
            end do
        end do
    end do

end subroutine fit_1fc

subroutine fit_2fc(spec, freqs, fc1, fc2, omega, n, oo, nn, ff, nf, res, gg, Y)
    implicit none
    ! Argument declarations
    integer :: ff, nn, oo, nf, gg
    double precision, intent(in) :: omega(oo), n(nn), spec(nf), freqs(nf)
    double precision, intent(in) :: fc1(ff), fc2(gg), Y
    double precision, intent(out) :: res(4)

    ! Variable declarations
    double precision :: synth(nf), best_ssq, ssq, weights(nf)
    integer :: i, j, k, l
    best_ssq = HUGE(ssq)
    do i = 1, ff
        do j = 1, nn
            do k = 1, oo
                do l = 1, gg
                    synth = log10(omega(k)) + (1./Y)*log10(1 + &
                    (freqs/fc2(l))**(n(j)*Y))
                    synth = synth - (1./Y)*log10(1 + (freqs/fc1(i))**(Y*n(j)))
                   ! synth = (1 + (freqs/fc2(l))**n(j))
                   ! synth = synth / (1 + (freqs/fc1(i))**n(j))
                   ! synth = synth * omega(k)
                    ssq = sum((synth-spec)**2)
                    if (ssq < best_ssq) then
                        best_ssq = ssq
                        res(1) = fc1(i)
                        res(2) = omega(k)
                        res(3) = n(j)
                        res(4) = fc2(l)
                    end if
                end do
            end do
        end do
    end do

end subroutine fit_2fc
