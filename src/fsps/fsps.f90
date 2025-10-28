module driver

  use sps_vars
  use sps_utils
  implicit none
  save

  !f2py intent(hide) pset
  type(PARAMS) :: pset

  !f2py intent(hide) ocompsp
  type(COMPSPOUT), dimension(ntfull) :: ocompsp

  integer :: is_setup=0

  !f2py intent(hide) has_ssp
  integer, dimension(nz) :: has_ssp=0

  !f2py intent(hide) has_ssp_age
  integer, dimension(nz,nt) :: has_ssp_age=0

  !f2py intent(hide) has_ssp_f
  integer, dimension(nz,nfbhb,nsbss,ndell,ndelt) :: has_ssp_f=0

  !f2py intent(hide) has_ssp_age_f
  integer, dimension(nz,nfbhb,nsbss,ndell,ndelt,nt) :: has_ssp_age_f=0

contains

  subroutine setup(compute_vega_mags0, vactoair_flag0)

    ! Load all the data files/templates into memory.

    implicit none

    integer, intent(in) :: compute_vega_mags0, vactoair_flag0


    compute_vega_mags = compute_vega_mags0
    vactoair_flag = vactoair_flag0
    call sps_setup(-1)
    is_setup = 1

    ! We will only compute mags when asked for through get_mags.
    pset%mag_compute=0

  end subroutine

  subroutine set_ssp_params(imf_type0,imf_upper_limit0,imf_lower_limit0,&
                            imf1,imf2,imf3,vdmc,mdave,dell,&
                            delt,sbss,fbhb,pagb,add_stellar_remnants0,&
                            tpagb_norm_type0,add_agb_dust_model0,agb_dust,&
                            redgb,agb,masscut,fcstar,evtype,use_wr_spectra0,&
                            logt_wmb_hot0,add_xrb_emission0,frac_xrb,bhbcomp,&
                            smooth_lsf0)

    ! Set the parameters that affect the SSP computation.

    implicit none

    integer, intent(in) :: imf_type0,add_stellar_remnants0,tpagb_norm_type0,&
                           add_agb_dust_model0,use_wr_spectra0,add_xrb_emission0,smooth_lsf0
    double precision, intent(in) :: imf_upper_limit0, imf_lower_limit0,&
                                    imf1,imf2,imf3,vdmc,mdave,dell,&
                                    delt,sbss,fbhb,pagb,agb_dust,&
                                    redgb,agb,masscut,fcstar,evtype,&
                                    logt_wmb_hot0,frac_xrb,bhbcomp

    imf_type=imf_type0
    imf_upper_limit=imf_upper_limit0
    imf_lower_limit=imf_lower_limit0
    add_stellar_remnants=add_stellar_remnants0
    tpagb_norm_type=tpagb_norm_type0
    add_agb_dust_model=add_agb_dust_model0
    use_wr_spectra=use_wr_spectra0
    logt_wmb_hot=logt_wmb_hot0
    smooth_lsf=smooth_lsf0
    add_xrb_emission=add_xrb_emission0
    pset%imf1=imf1
    pset%imf2=imf2
    pset%imf3=imf3
    pset%vdmc=vdmc
    pset%mdave=mdave
    pset%dell=dell
    pset%delt=delt
    pset%sbss=sbss
    pset%fbhb=fbhb
    pset%pagb=pagb
    pset%agb_dust=agb_dust
    pset%redgb=redgb
    pset%agb=agb
    pset%masscut=masscut
    pset%fcstar=fcstar
    pset%evtype=evtype
    pset%frac_xrb=frac_xrb
    ! pset%bhbcomp=bhbcomp

    has_ssp(:) = 0
    has_ssp_age(:,:) = 0

  end subroutine

  subroutine set_csp_params(smooth_velocity0,redshift_colors0,&
                            compute_light_ages0,nebemlineinspec0,&
                            dust_type0,add_dust_emission0,add_neb_emission0,&
                            add_neb_continuum0,cloudy_dust0,add_igm_absorption0,&
                            zmet,sfh,wgp1,wgp2,wgp3,tau,&
                            const,tage,fburst,tburst,dust1,dust2,dust3,&
                            logzsol,zred,pmetals,dust_clumps,frac_nodust,&
                            dust_index,dust_tesc,frac_obrun,uvb,mwr,&
                            dust1_index,sf_start,sf_trunc,sf_slope,&
                            duste_gamma,duste_umin,duste_qpah,&
                            sigma_smooth,min_wave_smooth,max_wave_smooth,&
                            gas_logu,gas_logz,igm_factor,fagn,agn_tau)

    ! Set all the parameters that don't affect the SSP computation.

    implicit none

    integer, intent(in) :: smooth_velocity0,redshift_colors0,&
                           compute_light_ages0,nebemlineinspec0,&
                           dust_type0,add_dust_emission0,add_neb_emission0,&
                           add_neb_continuum0,cloudy_dust0,add_igm_absorption0,&
                           zmet,sfh,wgp1,wgp2,wgp3
    double precision, intent(in) :: tau,&
                            const,tage,fburst,tburst,dust1,dust2,dust3,&
                            logzsol,zred,pmetals,dust_clumps,frac_nodust,&
                            dust_index,dust_tesc,frac_obrun,uvb,mwr,&
                            dust1_index,sf_start,sf_trunc,sf_slope,&
                            duste_gamma,duste_umin,duste_qpah,&
                            sigma_smooth,min_wave_smooth,max_wave_smooth,&
                            gas_logu,gas_logz,igm_factor,fagn,agn_tau

    smooth_velocity=smooth_velocity0
    redshift_colors=redshift_colors0
    compute_light_ages=compute_light_ages0
    nebemlineinspec=nebemlineinspec0
    dust_type=dust_type0
    add_dust_emission=add_dust_emission0
    add_neb_emission=add_neb_emission0
    add_neb_continuum=add_neb_continuum0
    cloudy_dust=cloudy_dust0
    add_igm_absorption=add_igm_absorption0

    pset%zmet=zmet
    pset%sfh=sfh
    pset%wgp1=wgp1
    pset%wgp2=wgp2
    pset%wgp3=wgp3

    pset%tau=tau
    pset%const=const
    pset%tage=tage
    pset%fburst=fburst
    pset%tburst=tburst
    pset%dust1=dust1
    pset%dust2=dust2
    pset%dust3=dust3
    pset%logzsol=logzsol
    pset%zred=zred
    pset%pmetals=pmetals
    pset%dust_clumps=dust_clumps
    pset%frac_nodust=frac_nodust
    pset%dust_index=dust_index
    pset%dust_tesc=dust_tesc
    pset%frac_obrun=frac_obrun
    pset%uvb=uvb
    pset%mwr=mwr
    pset%dust1_index=dust1_index
    pset%sf_start=sf_start
    pset%sf_trunc=sf_trunc
    pset%sf_slope=sf_slope
    pset%duste_gamma=duste_gamma
    pset%duste_umin=duste_umin
    pset%duste_qpah=duste_qpah
    pset%sigma_smooth=sigma_smooth
    pset%min_wave_smooth=min_wave_smooth
    pset%max_wave_smooth=max_wave_smooth
    pset%gas_logu=gas_logu
    pset%gas_logz=gas_logz
    pset%igm_factor=igm_factor
    pset%fagn=fagn
    pset%agn_tau=agn_tau

  end subroutine

  subroutine ssps

    ! Loop over the metallicity grid and compute all the SSPs.

    implicit none
    integer :: zi
    do zi=1,nz
      call ssp(zi)
    enddo

  end subroutine

  subroutine initialize_full_ssps()

    ! Loop over metallicity, fbhb, sbss, dell, and delt to compute all the SSPs.
    ! (this will take fucking forever...but it'll be worth it)

    implicit none 
    integer :: zi, fbhbi, sbssi, delli, delti, flat_index, stat0=1, stat1=1, stat2=1, stat3=1, i
    real(SP), dimension(27) :: stat_ssp_itp, stat_ssp_itp_0
    character(100) :: spec_cache
    character(100) :: mass_cache
    character(100) :: lbol_cache
    character(100) :: stat_cache
    character(5)   :: num, denom
    logical :: cache_exists, f1e, f2e, f3e, f4e, stat_equal

    write(*,*) 'Initializing the 6D SSP grid'

    ! Allocate big-ass arrays for all the SSPs
    allocate(spec_ssp_itp(nspec,ntfull,nz,nfbhb,nsbss,ndell,ndelt))
    allocate(mass_ssp_itp(ntfull,nz,nfbhb,nsbss,ndell,ndelt))
    allocate(lbol_ssp_itp(ntfull,nz,nfbhb,nsbss,ndell,ndelt))
    mass_ssp_itp=0.
    spec_ssp_itp=0.
    lbol_ssp_itp=0.

    ! Check if the cache file already exists
    spec_cache=trim(SPS_HOME)//'/OUTPUTS/spec_ssp_itp.cache'
    mass_cache=trim(SPS_HOME)//'/OUTPUTS/mass_ssp_itp.cache'
    lbol_cache=trim(SPS_HOME)//'/OUTPUTS/lbol_ssp_itp.cache'
    stat_cache=trim(SPS_HOME)//'/OUTPUTS/stat_ssp_itp.cache'
    inquire(file=spec_cache, exist=f1e)
    inquire(file=mass_cache, exist=f2e)
    inquire(file=lbol_cache, exist=f3e)
    inquire(file=stat_cache, exist=f4e)
    cache_exists = f1e .and. f2e .and. f3e .and. f4e

    ! save important information about how the isochrones should be generated based on
    ! current values of the parameters that could change the resultant SSPs:
    stat_ssp_itp(1)=real(imf_type)
    stat_ssp_itp(2)=imf_upper_limit
    stat_ssp_itp(3)=imf_lower_limit
    stat_ssp_itp(4)=real(add_stellar_remnants)
    stat_ssp_itp(5)=real(tpagb_norm_type)
    stat_ssp_itp(6)=real(add_agb_dust_model)
    stat_ssp_itp(7)=real(use_wr_spectra)
    stat_ssp_itp(8)=logt_wmb_hot
    stat_ssp_itp(9)=real(smooth_lsf)
    stat_ssp_itp(10)=real(add_xrb_emission)
    stat_ssp_itp(11)=pset%imf1
    stat_ssp_itp(12)=pset%imf2
    stat_ssp_itp(13)=pset%imf3
    stat_ssp_itp(14)=pset%vdmc
    stat_ssp_itp(15)=pset%mdave
    stat_ssp_itp(16)=pset%dell
    stat_ssp_itp(17)=pset%delt
    stat_ssp_itp(18)=pset%sbss
    stat_ssp_itp(19)=pset%fbhb
    stat_ssp_itp(20)=pset%pagb
    stat_ssp_itp(21)=pset%agb_dust
    stat_ssp_itp(22)=pset%redgb
    stat_ssp_itp(23)=pset%agb
    stat_ssp_itp(24)=pset%masscut
    stat_ssp_itp(25)=pset%fcstar
    stat_ssp_itp(26)=pset%evtype
    stat_ssp_itp(27)=pset%frac_xrb
    ! stat_ssp_itp(28)=pset%bhbcomp

    if (.not.cache_exists) then
      write(*,*) 'No cache exists'
101   write(*,*) 'A new grid will be calculated (this may take a while).'
      pset%ssp_gen_age = 1
      flat_index = 1
      do delti=1,ndelt
        do delli=1,ndell 
          do sbssi=1,nsbss 
            do fbhbi=1,nfbhb 
              do zi=1,nz
                !omfg quintuply nested for loop o.O
                write(num,   '(I0)') flat_index 
                write(denom, '(I0)') nz*nfbhb*nsbss*ndell*ndelt
                write(*, '(A)', advance='no') '('//num//'/'//denom//')'//char(13)
                call ssp_f(zi,fbhbi,sbssi,delli,delti)
                flat_index = flat_index + 1
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) ''
      write(*,*) 'Done!'

      write(*,*) 'Saving results to cache files: '//stat_cache//', '//spec_cache//', '//mass_cache//', '//lbol_cache 
      open(96, file=stat_cache, access='stream', form='unformatted', status='new', iostat=stat0, action='write')
      open(97, file=spec_cache, access='stream', form='unformatted', status='new', iostat=stat1, action='write')
      open(98, file=mass_cache, access='stream', form='unformatted', status='new', iostat=stat2, action='write')
      open(99, file=lbol_cache, access='stream', form='unformatted', status='new', iostat=stat3, action='write')
      if (stat0.ne.0.or.stat1.ne.0.or.stat2.ne.0.or.stat3.ne.0) then
        write(*,*) 'ssps_f error: cache files cannot be opened: '//stat_cache//', '//spec_cache//', '//mass_cache//', '//lbol_cache
        stop
      endif
      write(96) stat_ssp_itp
      write(97) spec_ssp_itp
      write(98) mass_ssp_itp
      write(99) lbol_ssp_itp
      close(96)
      close(97)
      close(98)
      close(99)
      write(*,*) 'Results saved successfully.'

    else
      write(*,*) 'A cache exists. Checking to see if relevant SSP parameters match.'

      open(96, file=stat_cache, access='stream', form='unformatted', status='old', iostat=stat0, action='read')
      if (stat0.ne.0) then 
        write(*,*) 'ssps_f error: cache file cannot be opened: '//stat_cache 
        stop 
      endif
      read(96) stat_ssp_itp_0
      close(96) 

      ! check that all of the stat parameters in the cache match the current ones 
      stat_equal = .true.
      do i=1,27
        if (stat_ssp_itp_0(i) /= stat_ssp_itp(i)) stat_equal = .false.
      enddo

      if (.not. stat_equal) then 
        write(*,*) 'SSP parameters do not match.'
        goto 101
      endif

      write(*,*) 'SSP parameters match. Reading 6D SSP grid from cache files: '//spec_cache//', '//mass_cache//', '//lbol_cache
      open(97, file=spec_cache, access='stream', form='unformatted', status='old', iostat=stat1, action='read')
      open(98, file=mass_cache, access='stream', form='unformatted', status='old', iostat=stat2, action='read')
      open(99, file=lbol_cache, access='stream', form='unformatted', status='old', iostat=stat3, action='read')
      if (stat1.ne.0.or.stat2.ne.0.or.stat3.ne.0) then
        write(*,*) 'ssps_f error: cache files cannot be opened: '//spec_cache//', '//mass_cache//', '//lbol_cache
        stop
      endif
      read(97) spec_ssp_itp
      read(98) mass_ssp_itp
      read(99) lbol_ssp_itp
      close(97)
      close(98)
      close(99)
      write(*,*) 'Results read successfully.'

      ! Set flags accordingly
      has_ssp_f = 1
      has_ssp_age_f = 1

    endif

    write(*,*) 'SSPs have finished initializing.'

  end subroutine

  subroutine ssp(zi)

    ! Compute a SSP at a single metallicity.

    implicit none
    integer, intent(in) :: zi
    pset%zmet = zi
    call ssp_gen(pset, mass_ssp_zz(:,zi),lbol_ssp_zz(:,zi),&
         spec_ssp_zz(:,:,zi))
    if (minval(pset%ssp_gen_age) .eq. 1) then
       has_ssp(zi) = 1
    endif
    has_ssp_age(zi,:) = pset%ssp_gen_age

  end subroutine

  subroutine ssp_f(zi, fbhbi, sbssi, delli, delti)

    ! Compute a SSP at a single metallicity, fbhb, sbss, dell, and delt

    implicit none
    integer, intent(in) :: zi, fbhbi, sbssi, delli, delti
    integer  :: zmet0
    real(SP) :: fbhb0, sbss0, dell0, delt0

    ! save the original values in the pset 
    zmet0 = pset%zmet 
    fbhb0 = pset%fbhb 
    sbss0 = pset%sbss 
    dell0 = pset%dell 
    delt0 = pset%delt

    ! set the new values
    pset%zmet = zi
    pset%fbhb = fbhb_legend(fbhbi)
    pset%sbss = sbss_legend(sbssi)
    pset%dell = dell_legend(delli)
    pset%delt = delt_legend(delti)

    call ssp_gen(pset, mass_ssp_itp(:,zi,fbhbi,sbssi,delli,delti),lbol_ssp_itp(:,zi,fbhbi,sbssi,delli,delti),&
                 spec_ssp_itp(:,:,zi,fbhbi,sbssi,delli,delti))
    if (minval(pset%ssp_gen_age) .eq. 1) then 
      has_ssp_f(zi,fbhbi,sbssi,delli,delti) = 1
    endif
    has_ssp_age_f(zi,fbhbi,sbssi,delli,delti,:) = pset%ssp_gen_age 

    ! and reset back to the original values
    pset%zmet = zmet0 
    pset%fbhb = fbhb0
    pset%sbss = sbss0
    pset%dell = dell0 
    pset%delt = delt0
  
  end subroutine

  subroutine compute_zdep(ns,n_age,ztype)

    ! Compute the full CSP (and the SSPs if they aren't already cached).
    ! After interpolation in metallicity

    implicit none
    integer, intent(in) :: ns,n_age,ztype
    double precision, dimension(ns,n_age) :: spec
    double precision, dimension(n_age) :: mass,lbol
    integer :: zlo,zmet
    double precision :: zpos
    character(100) :: outfile

    if (ztype .eq. 0) then
       ! Build the SSP for one metallicity, then feed to compsp
       zmet = pset%zmet
       if (has_ssp(zmet) .eq. 0) then
          call ssp(zmet)
       endif
       !mass = mass_ssp_zz(:,zmet)
       !lbol = lbol_ssp_zz(:,zmet)
       !spec = spec_ssp_zz(:,:,zmet)
       call compsp(0,1,outfile,mass_ssp_zz(:,zmet),lbol_ssp_zz(:,zmet),&
            spec_ssp_zz(:,:,zmet),pset,ocompsp)
    endif

    if (ztype .eq. 1) then
       zpos = pset%logzsol
       ! Find the bracketing metallicity indices and generate ssps if
       ! necessary, then interpolate, and feed the result to compsp
       zlo = max(min(locate(log10(zlegend/zsol),zpos),nz-1),1)
       do zmet=zlo,zlo+1
          if (has_ssp(zmet) .eq. 0) then
             call ssp(zmet)
          endif
       enddo
       call ztinterp(zpos,spec,lbol,mass)
       call compsp(0,1,outfile,mass,lbol,spec,pset,ocompsp)
    endif

    if (ztype .eq. 2) then
       zpos = pset%logzsol
       ! Build the SSPs for *every* metallicity if necessary, then
       ! comvolve with the MDF, and then feed to compsp
       do zmet=1,nz
          if (has_ssp(zmet) .eq. 0) then
             call ssp(zmet)
          endif
       enddo
       call ztinterp(zpos,spec,lbol,mass,zpow=pset%pmetals)
       call compsp(0,1,outfile,mass,lbol,spec,pset,ocompsp)
    endif

    if (ztype .eq. 3) then
       ! Build the SSPs for *every* metallicity and feed all of them to compsp
       ! for z-dependent tabular
       do zmet=1,nz
          if (has_ssp(zmet) .eq. 0) then
             call ssp(zmet)
          endif
       enddo
       call compsp(0,nz,outfile,mass_ssp_zz,lbol_ssp_zz,&
            spec_ssp_zz,pset,ocompsp)
    endif


  end subroutine

  subroutine compute_fdep(ns,n_age)

    ! Compute the full CSP (and the SSPs if they aren't already cached).
    ! After interpolation in metallicity, fbhb, sbss, dell, and delt

    implicit none
    integer, intent(in) :: ns,n_age
    double precision, dimension(ns,n_age) :: spec
    double precision, dimension(n_age) :: mass,lbol
    integer :: zlo,zi,fbhblo,fbhbi,sbsslo,sbssi,delllo,delli,deltlo,delti,flat_index
    double precision :: zpos, fbhbpos, sbsspos, dellpos, deltpos
    character(100) :: outfile

    zpos = pset%logzsol
    fbhbpos = pset%fbhb 
    sbsspos = pset%sbss 
    dellpos = pset%dell 
    deltpos = pset%delt

    ! Find the bracketing metallicity indices and generate ssps if
    ! necessary, then interpolate, and feed the result to compsp
    zlo    = max(min(locate(log10(zlegend/zsol),zpos),nz-1),1)
    fbhblo = max(min(locate(fbhb_legend,fbhbpos),  nfbhb-1),1) 
    sbsslo = max(min(locate(sbss_legend,sbsspos),  nsbss-1),1)
    delllo = max(min(locate(dell_legend,dellpos),  ndell-1),1)
    deltlo = max(min(locate(delt_legend,deltpos),  ndelt-1),1)

    ! Calculate SSPs if they aren't already cached
    do zi=zlo,zlo+1
      do fbhbi=fbhblo,fbhblo+1
        do sbssi=sbsslo,sbsslo+1
          do delli=delllo,delllo+1
            do delti=deltlo,deltlo+1
              if (has_ssp_f(zi,fbhbi,sbssi,delli,delti) .eq. 0) then
                  call ssp_f(zi,fbhbi,sbssi,delli,delti)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo

    ! Interpolate and compute the CSP
    call zfinterp(zpos,fbhbpos,sbsspos,dellpos,deltpos,spec,lbol,mass)
    call compsp(0,1,outfile,mass,lbol,spec,pset,ocompsp)

  end subroutine

  subroutine get_spec(ns,n_age,spec_out)

    ! Get the grid of spectra for the computed CSP at all ages.

    implicit none
    integer :: i
    integer, intent(in) :: ns,n_age
    double precision, dimension(n_age,ns), intent(out) :: spec_out
    do i=1,n_age
      spec_out(i,:) = ocompsp(i)%spec
    enddo

  end subroutine

  subroutine get_mags(ns,n_age,n_bands,z_red,mc,mags)

    ! Get the photometric magnitudes in all the recognized bands.

    implicit none
    integer :: i
    integer, intent(in) :: ns, n_age, n_bands
    double precision, intent(in) :: z_red
    integer, dimension(n_bands), intent(in) :: mc
    double precision, dimension(n_age,n_bands), intent(out) :: mags
    double precision, dimension(ns) :: tspec
    do i=1,n_age
      tspec = ocompsp(i)%spec
      call getmags(z_red,tspec,mags(i,:),mc)
    enddo

  end subroutine

  subroutine interp_ssp(ns,zpos,tpos,spec,mass,lbol)

    ! Return the SSPs interpolated to the target metallicity
    !(zpos) and target age (tpos)

    implicit none

    integer, intent(in) :: ns
    double precision, intent(in) :: zpos
    double precision, intent(in) :: tpos

    double precision, dimension(ns,1), intent(inout) :: spec
    double precision, dimension(1), intent(inout) :: mass,lbol

    double precision, dimension(nt) :: time

    integer :: zlo,zmet,tlo

    zlo = max(min(locate(log10(zlegend/0.0190),zpos),nz-1),1)
    time = timestep_isoc(zlo,:)
    tlo = max(min(locate(time,tpos),nt-1),1)

    do zmet=zlo,zlo+1
       if ((has_ssp_age(zmet,tlo) .eq. 0) .or. (has_ssp_age(zmet,tlo+1) .eq. 0)) then
          pset%ssp_gen_age = 0
          pset%ssp_gen_age(tlo:tlo+1) = 1
          call ssp(zmet)
          pset%ssp_gen_age = 1
       endif
    enddo

    call ztinterp(zpos,spec,lbol,mass,tpos=tpos)

    end subroutine

    subroutine interp_ssp_f(ns,zpos,tpos,fbhbpos,sbsspos,dellpos,deltpos,spec,mass,lbol)

    ! Return the SSPs interpolated to the target 
    ! metallicity (zpos),
    ! age (tpos),
    ! fbhb (fbhbpos),
    ! sbss (sbsspos),
    ! dell (dellpos),
    ! and delt (deltpos)

    implicit none

    integer, intent(in) :: ns
    double precision, intent(in) :: zpos, tpos, fbhbpos, sbsspos, dellpos, deltpos

    double precision, dimension(ns,1), intent(inout) :: spec
    double precision, dimension(1), intent(inout) :: mass,lbol

    double precision, dimension(nt) :: time

    integer :: tlo,zlo,zi,fbhblo,fbhbi,sbsslo,sbssi,delllo,delli,deltlo,delti,flat_index

    tlo = max(min(locate(time,tpos),nt-1),1)
    time = timestep_isoc(zlo,:)

    zlo    = max(min(locate(log10(zlegend/0.0190),zpos),nz-1),1)
    fbhblo = max(min(locate(fbhb_legend,fbhbpos),    nfbhb-1),1) 
    sbsslo = max(min(locate(sbss_legend,sbsspos),    nsbss-1),1)
    delllo = max(min(locate(dell_legend,dellpos),    ndell-1),1)
    deltlo = max(min(locate(delt_legend,deltpos),    ndelt-1),1) 

    do zi=zlo,zlo+1
      do fbhbi=fbhblo,fbhblo+1
        do sbssi=sbsslo,sbsslo+1
          do delli=delllo,delllo+1
            do delti=deltlo,deltlo+1
              if ((has_ssp_age_f(zi,fbhbi,sbssi,delli,delti,tlo) .eq. 0) .or. &
                  (has_ssp_age_f(zi,fbhbi,sbssi,delli,delti,tlo+1) .eq. 0)) then
                  pset%ssp_gen_age = 0
                  pset%ssp_gen_age(tlo:tlo+1) = 1
                  call ssp_f(zi,fbhbi,sbssi,delli,delti)
                  pset%ssp_gen_age = 1
              endif
            enddo
          enddo
        enddo
      enddo
    enddo

    call zfinterp(zpos,fbhbpos,sbsspos,dellpos,deltpos,spec,lbol,mass,tpos=tpos)

    end subroutine

    subroutine smooth_spectrum(ns,wave,spec,sigma_broad,minw,maxw)

    ! Smooth the spectrum by a gaussian of width sigma_broad

    implicit none
    integer, intent(in) :: ns
    double precision, intent(in) :: sigma_broad,minw,maxw
    double precision, dimension(ns), intent(in) :: wave
    double precision, dimension(ns), intent(inout) :: spec

    call smoothspec(wave,spec,sigma_broad,minw,maxw)

  end subroutine

  subroutine stellar_spectrum(ns,mact,logt,lbol,logg,phase,ffco,lmdot,wght,spec_out)

    ! Get a stellar spectrum for a given set of parameters

    implicit none
    integer :: i
    integer, intent(in) :: ns
    double precision, intent(in) :: mact, logt, lbol, logg, phase, ffco, lmdot, wght
    double precision, dimension(ns), intent(inout) :: spec_out

    call getspec(pset,mact,logt,lbol,logg,phase,ffco,lmdot,wght,spec_out)

  end subroutine

  subroutine get_ssp_weights(n_age, n_z, ssp_wghts_out)

    ! Return the weights of each SSP in the CSP

    implicit none
    integer, intent(in) :: n_age,n_z
    double precision, dimension(n_age,n_z), intent(inout) :: ssp_wghts_out
    ssp_wghts_out = weight_ssp

  end subroutine

  subroutine get_csp_components(ns, csp1, csp2)

    ! Return the unattenuated 'young' and 'old' stellar continuua

    implicit none
    integer, intent(in) :: ns
    double precision, dimension(ns), intent(inout) :: csp1, csp2
    csp1 = spec_young
    csp2 = spec_old

  end subroutine


  subroutine get_ssp_spec(ns,n_age,n_z,ssp_spec_out,ssp_mass_out,ssp_lbol_out)

    ! Return the contents of the ssp spectral array,
    ! regenerating the ssps if necessary

    implicit none
    integer, intent(in) :: ns,n_age,n_z
    integer :: zi
    double precision, dimension(ns,n_age,n_z), intent(inout) :: ssp_spec_out
    double precision, dimension(n_age,n_z), intent(inout) :: ssp_mass_out, ssp_lbol_out
    do zi=1,nz
       if (has_ssp(zi) .eq. 0) then
          call ssp(zi)
       endif
    enddo

    ssp_spec_out = spec_ssp_zz
    ssp_mass_out = mass_ssp_zz
    ssp_lbol_out = lbol_ssp_zz

  end subroutine

  subroutine get_ssp_spec_f(ns,n_age,n_z,n_fbhb,n_sbss,n_dell,n_delt,ssp_spec_out,ssp_mass_out,ssp_lbol_out)

    ! Return the contents of the ssp spectral array,
    ! regenerating the ssps if necessary

    implicit none
    integer, intent(in) :: ns, n_age, n_z, n_fbhb, n_sbss, n_dell, n_delt
    integer :: zi, fbhbi, sbssi, delli, delti
    double precision, dimension(ns,n_age,n_z,n_fbhb,n_sbss,n_dell,n_delt), intent(inout) :: ssp_spec_out
    double precision, dimension(n_age,n_z,n_fbhb,n_sbss,n_dell,n_delt), intent(inout)    :: ssp_mass_out, ssp_lbol_out

    do delti=1,ndelt
      do delli=1,ndell
        do sbssi=1,nsbss
          do fbhbi=1,nfbhb
            do zi=1,nz
              if (has_ssp_f(zi,fbhbi,sbssi,delli,delti) .eq. 0) then
                  call ssp_f(zi,fbhbi,sbssi,delli,delti)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo

    ssp_spec_out = spec_ssp_itp
    ssp_mass_out = mass_ssp_itp
    ssp_lbol_out = lbol_ssp_itp

  end subroutine

  subroutine set_sfh_tab(ntab, age, sfr, met)

    ! Fill the sfh_tab array

    implicit none
    integer, intent(in) :: ntab
    double precision, dimension(ntab), intent(in) :: age, sfr, met
    ntabsfh = ntab
    sfh_tab(1,1:ntabsfh) = age
    sfh_tab(2, 1:ntabsfh) = sfr
    sfh_tab(3, 1:ntabsfh) = met

  end subroutine

  subroutine set_ssp_lsf(nsv, sigma, wlo, whi)

    ! Fill the lsfinfo structure

    implicit none
    integer, intent(in) :: nsv
    double precision, dimension(nsv), intent(in) :: sigma
    double precision, intent(in) :: wlo, whi
    lsfinfo%minlam = wlo
    lsfinfo%maxlam = whi
    lsfinfo%lsf = sigma

  end subroutine

  subroutine get_setup_vars(cvms, vta_flag)

    implicit none
    integer, intent(out) :: cvms, vta_flag
    cvms = compute_vega_mags
    vta_flag = vactoair_flag

  end subroutine

  subroutine get_nz(n_z)

    ! Get the number of metallicity bins (hard coded in sps_vars).
    implicit none
    integer, intent(out) :: n_z
    n_z = nz

  end subroutine

  subroutine get_zlegend(n_z,z_legend)

    ! Get the available metallicity values.
    implicit none
    integer, intent(in) :: n_z
    double precision, dimension(n_z), intent(out) :: z_legend
    z_legend = zlegend

  end subroutine

  subroutine get_zsol(z_sol)

    ! Get the definition of solar metallicity.
    implicit none
    double precision, intent(out) :: z_sol
    z_sol = zsol

  end subroutine

  subroutine get_timefull(n_age,timefull)

    ! Get the actual time steps of the SSPs.
    implicit none
    integer, intent(in) :: n_age
    double precision, dimension(n_age), intent(out) :: timefull

    timefull = time_full

  end subroutine

  subroutine get_ntfull(n_age)

    ! Get the total number of time steps (hard coded in sps_vars).
    implicit none
    integer, intent(out) :: n_age
    n_age = ntfull

  end subroutine

  subroutine get_nspec(ns)

    ! Get the number of wavelength bins in the spectra (hard coded in
    ! sps_vars).
    implicit none
    integer, intent(out) :: ns
    ns = nspec

  end subroutine

  subroutine get_nfbhb(n_fbhb)

    implicit none 
    integer, intent(out) :: n_fbhb 
    n_fbhb = nfbhb 

  end subroutine

  subroutine get_nsbss(n_sbss)

    implicit none 
    integer, intent(out) :: n_sbss 
    n_sbss = nsbss 

  end subroutine

  subroutine get_ndell(n_dell)

    implicit none 
    integer, intent(out) :: n_dell 
    n_dell = ndell 

  end subroutine

  subroutine get_ndelt(n_delt)

    implicit none 
    integer, intent(out) :: n_delt
    n_delt = ndelt 

  end subroutine

  subroutine get_nbands(nb)

    ! Get the number of known filters (hard coded in sps_vars).
    implicit none
    integer, intent(out) :: nb
    nb = nbands

  end subroutine

  subroutine get_nemline(nline)

    ! Get the number of emission lines (hard coded in sps_vars).
    implicit none
    integer, intent(out) :: nline
    nline = nemline

  end subroutine

  subroutine get_emlambda(nline,em_lambda)

    ! Get the emission line wavelengths
    implicit none
    integer, intent(in) :: nline
    double precision, dimension(nline), intent(out) :: em_lambda
    if (vactoair_flag .eq. 1) then
       em_lambda = vactoair(nebem_line_pos)
    else
       em_lambda = nebem_line_pos
    endif

  end subroutine

  subroutine get_lambda(ns,lambda)

    ! Get the grid of wavelength bins.
    implicit none
    integer, intent(in) :: ns
    double precision, dimension(ns), intent(out) :: lambda
    if (vactoair_flag .eq. 1) then
       lambda = vactoair(spec_lambda)
    else
       lambda = spec_lambda
    endif

  end subroutine

  subroutine get_res(ns,res)

    ! Get the resolution array of the spectral library
    implicit none
    integer, intent(in) :: ns
    double precision, dimension(ns), intent(out) :: res
    res = spec_res

  end subroutine

  subroutine get_libraries(isocname,specname,dustname)

    implicit none

    character(4), intent(out) :: isocname
    character(5), intent(out) :: specname
    character(6), intent(out) :: dustname
    isocname = isoc_type
    specname = spec_type
    dustname = str_dustem

  end subroutine

  subroutine get_isochrone_dimensions(n_age,n_mass)

    implicit none

    ! Get the dimensions of the produced isochrones.
    integer, intent(out) :: n_age,n_mass
    n_age = nt
    n_mass = n_mass

  end subroutine

  subroutine get_nmass_isochrone(zz, tt, nmass)

    implicit none

    ! Get the number of masses included in a specific isochrone.
    integer, intent(in) :: zz,tt
    integer, intent(out) :: nmass
    nmass = nmass_isoc(zz,tt)

  end subroutine

  subroutine get_stats(n_age,nline,age,mass_csp,lbol_csp,sfr,mdust,mformed,emlines)

    implicit none

    ! Get some stats about the computed SP.
    integer :: i
    integer, intent(in) :: n_age,nline
    double precision, dimension(n_age), intent(out) :: age,mass_csp,&
                                                       lbol_csp,sfr,mdust,&
                                                       mformed
    double precision, dimension(n_age,nline), intent(out) :: emlines

    do i=1,n_age
      age(i)      = ocompsp(i)%age
      mass_csp(i) = ocompsp(i)%mass_csp
      lbol_csp(i) = ocompsp(i)%lbol_csp
      sfr(i)      = ocompsp(i)%sfr
      mdust(i)    = ocompsp(i)%mdust
      mformed(i)  = ocompsp(i)%mformed
      emlines(i,:)  = ocompsp(i)%emlines
    enddo

  end subroutine

  subroutine get_filter_data(nb, wave_eff, mag_vega, mag_sun)

    !get info about the filters
    implicit none
    integer, intent(in) :: nb
    double precision, dimension(nb), intent(out) :: wave_eff,mag_vega,mag_sun
    wave_eff = filter_leff
    mag_vega = magvega - magvega(1)
    mag_sun = magsun

  end subroutine

  subroutine write_isoc(outfile)

    implicit none

    character(100), intent(in)  :: outfile

    call write_isochrone(outfile, pset)

  end subroutine

end module
