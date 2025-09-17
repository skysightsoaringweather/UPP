      SUBROUTINE DE_ALLOCATE
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     SETS UP MESSAGE PASSING INFO
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-19  MIKE BALDWIN - WRF VERSION
!
! USAGE:    CALL MPI_FIRST
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!       PARA_RANGE
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON - CTLBLK.comm
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
!
      use vrbls4d
      use vrbls3d
      use vrbls2d
      use soil
      use masks
      use params_mod
      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
       implicit none
!
      include 'mpif.h'
!
      logical minimal_memory
      character(len=10) :: env_minimal_mem
!
! Check for minimal memory mode
      minimal_memory = .false.
      call get_environment_variable("UPP_MINIMAL_MEMORY", env_minimal_mem)
      if (trim(env_minimal_mem) == "YES" .or. trim(env_minimal_mem) == "1") then
        minimal_memory = .true.
      endif
!
!     deallocate arrays
!
!
!     FROM VRBLS3D
!
      deallocate(u)
      deallocate(v)
      deallocate(t)
! CHUANG ADD POTENTIAL TEMP BECAUSE WRF OUTPUT THETA
!      deallocate(th)   
      deallocate(q)
!      deallocate(w(im,jsta_2l:jend_2u,lp1))
      deallocate(uh)
      deallocate(vh)
      deallocate(wh)
      deallocate(pmid)
      deallocate(pmidv)
      deallocate(pint)
      deallocate(alpint)
      deallocate(zmid)
      deallocate(zint)
!      deallocate(rainw(im,jsta_2l:jend_2u,lm))
      deallocate(q2)
      deallocate(omga)
      deallocate(T_ADJ)
      deallocate(ttnd)
      deallocate(rswtt)
      deallocate(rlwtt)
      deallocate(exch_h) 
      deallocate(train)
      deallocate(tcucn)
      deallocate(EL_PBL)
!     MP FIELD   
      deallocate(cwm)
      if (.not. minimal_memory) then
        if (allocated(F_ice)) deallocate(F_ice)
        if (allocated(F_rain)) deallocate(F_rain)
        if (allocated(F_RimeF)) deallocate(F_RimeF)
        if (allocated(QQW)) deallocate(QQW)
        if (allocated(QQI)) deallocate(QQI)
        if (allocated(QQR)) deallocate(QQR)
        if (allocated(QQS)) deallocate(QQS)
        if (allocated(QQG)) deallocate(QQG)
        if (allocated(QQNW)) deallocate(QQNW)
        if (allocated(QQNI)) deallocate(QQNI)
        if (allocated(QQNR)) deallocate(QQNR)
        if (allocated(QQNWFA)) deallocate(QQNWFA)
        if (allocated(QQNIFA)) deallocate(QQNIFA)
      endif
      if (.not. minimal_memory) then
        if (allocated(TAOD5503D)) deallocate(TAOD5503D)
        if (allocated(AEXTC55)) deallocate(AEXTC55)
        if (allocated(EXTCOF55)) deallocate(EXTCOF55)
      endif
      deallocate(CFR)
      deallocate(CFR_RAW)
      deallocate(DBZ)
      deallocate(DBZR)
      deallocate(DBZI)
      deallocate(DBZC)
      deallocate(mcvg)
      deallocate(NLICE)
!     Wm Lewis: added
      deallocate(NRAIN)
      deallocate(radius_cloud)
      deallocate(radius_ice)
      deallocate(radius_snow)
!GFS FIELD
      deallocate(o3)
      deallocate(o)
      deallocate(o2)
      deallocate(tcucns)
! Add GFS d3d fields
      if (d3d_on) then
        deallocate(vdifftt)
!       deallocate(tcucns)
        deallocate(vdiffmois)
        deallocate(dconvmois)
        deallocate(sconvmois)
        deallocate(nradtt)
        deallocate(o3vdiff)
        deallocate(o3prod)
        deallocate(o3tndy)
        deallocate(mwpv)
        deallocate(unknown)
        deallocate(vdiffzacce)
        deallocate(zgdrag)
        deallocate(cnvctummixing)
        deallocate(vdiffmacce)
        deallocate(mgdrag)
        deallocate(cnvctvmmixing)
        deallocate(ncnvctcfrac)
        deallocate(cnvctumflx)
        deallocate(cnvctdmflx)
        deallocate(cnvctdetmflx)
        deallocate(cnvctzgdrag)
        deallocate(cnvctmgdrag)      
      endif
!
!     FROM SOIL
!
      deallocate(smc)
      deallocate(stc)
      deallocate(sh2o)
      deallocate(SLDPTH)
      deallocate(RTDPTH)
      deallocate(SLLEVEL)
!
!     FROM VRBLS2D
!
      deallocate(u10)
      deallocate(v10)
      deallocate(tshltr)
      deallocate(qshltr)
      deallocate(mrshltr)
      deallocate(smstav)
      deallocate(ssroff)
      deallocate(bgroff)
      deallocate(vegfrc)
      deallocate(acsnow)
      deallocate(acgraup)
      deallocate(acfrain)
      deallocate(acsnom)
      deallocate(cmc)
      deallocate(sst)
      deallocate(qz0)
      deallocate(thz0)
      deallocate(uz0)
      deallocate(vz0)
      deallocate(qs)
      deallocate(ths)
      deallocate(sno)
      deallocate(snonc)
      deallocate(snoavg)
      deallocate(psfcavg)
      deallocate(t10m)
      deallocate(t10avg)
      deallocate(akhsavg)
      deallocate(akmsavg)
      deallocate(u10max)
      deallocate(v10max)
      deallocate(u10h)
      deallocate(v10h)
      deallocate(akms)
      deallocate(akhs)
      deallocate(cuprec)
      deallocate(acprec)
      deallocate(ancprc)
      deallocate(cuppt)
      deallocate(tsnow)
      deallocate(qvg)
      deallocate(qv2m)
      deallocate(rswin)
      deallocate(swddni)
      deallocate(swddif)
      deallocate(swdnbc)
      deallocate(swupbc)
      deallocate(swddnic)
      deallocate(swddifc)
      deallocate(swupt)
      deallocate(int_smoke)
      deallocate(mean_frp)
      deallocate(int_aod)
      deallocate(smoke)
      deallocate(taod5502d)
      deallocate(aerasy2d)
      deallocate(aerssa2d)
      deallocate(lwp)
      deallocate(iwp)
      deallocate(rlwin)
      deallocate(lwdnbc)
      deallocate(lwupbc)
      deallocate(rlwtoa)
      deallocate(rswtoa)
      deallocate(tg)
      deallocate(sfcshx)
      deallocate(sfclhx)
      deallocate(fis)
      deallocate(t500)
      deallocate(cfracl)
      deallocate(cfracm)
      deallocate(cfrach)
      deallocate(acfrst)
      deallocate(acfrcv)
      deallocate(hbot)
      deallocate(htop)
      deallocate(aswin)
      deallocate(alwin)
      deallocate(aswout)
      deallocate(alwout)
      deallocate(aswtoa)
      deallocate(alwtoa)
      deallocate(czen)
      deallocate(czmean)
      deallocate(sigt4)
      deallocate(rswout)
      deallocate(radot)
      deallocate(ncfrst)  ! real
      deallocate(ncfrcv)  ! real
      deallocate(smstot)
      deallocate(pctsno)
      deallocate(pshltr)
      deallocate(th10)
      deallocate(q10)
      deallocate(sr)
      deallocate(prec)
      deallocate(subshx)
      deallocate(snopcx)
      deallocate(sfcuvx)
      deallocate(sfcevp)
      deallocate(potevp)
      deallocate(z0)
      deallocate(ustar)
      deallocate(pblh)
      deallocate(pblhgust)
      deallocate(twbs)
      deallocate(qwbs)
      deallocate(sfcexc)
      deallocate(grnflx)
      deallocate(soiltb)
      deallocate(z1000)
      deallocate(slp)
      deallocate(pslp)
      deallocate(f)
      deallocate(albedo)
      deallocate(albase)
      deallocate(cldfra)
      deallocate(cprate)
      deallocate(cnvcfr)
      deallocate(ivgtyp)
      deallocate(isltyp)
      deallocate(hbotd)
      deallocate(htopd)
      deallocate(hbots)
      deallocate(htops)
      deallocate(cldefi)
      deallocate(islope)
      deallocate(si)
      deallocate(lspa)
      deallocate(rswinc)
      deallocate(vis)
      deallocate(pd)
      deallocate(mxsnal)
! add GFS fields
      deallocate(sfcux)
      deallocate(sfcvx)
      deallocate(sfcuxi)
      deallocate(sfcvxi)
      deallocate(avgalbedo)
      deallocate(avgcprate)
      deallocate(avgprec)
      deallocate(avgprec_cont)
      deallocate(avgcprate_cont)
      deallocate(ptop)
      deallocate(pbot)
      deallocate(avgcfrach)
      deallocate(avgcfracm)
      deallocate(avgcfracl)
      deallocate(avgtcdc)
      deallocate(auvbin)
      deallocate(auvbinc)
      deallocate(ptopl)
      deallocate(pbotl)
      deallocate(Ttopl)
      deallocate(ptopm)
      deallocate(pbotm)
      deallocate(Ttopm)
      deallocate(ptoph)
      deallocate(pboth)
      deallocate(Ttoph)
      deallocate(sfcugs)
      deallocate(sfcvgs)
      deallocate(pblcfr)
      deallocate(cldwork)
      deallocate(gtaux)
      deallocate(gtauy)
      deallocate(mdltaux)
      deallocate(mdltauy)
      deallocate(runoff)
      deallocate(maxtshltr)
      deallocate(mintshltr)
      deallocate(maxrhshltr)
      deallocate(minrhshltr)
      deallocate(maxqshltr)
      deallocate(minqshltr)
      deallocate(mixht)
      deallocate(epsr)
      deallocate(dzice)
      deallocate(alwinc)
      deallocate(alwoutc)
      deallocate(alwtoac)
      deallocate(aswinc)
      deallocate(aswoutc)
      deallocate(aswtoac)
      deallocate(aswintoa)
      deallocate(smcwlt)
      deallocate(suntime)
      deallocate(fieldcapa)
      deallocate(avisbeamswin)
      deallocate(avisdiffswin)
      deallocate(airbeamswin)
      deallocate(airdiffswin)
      deallocate(snowfall)
      deallocate(acond)
      deallocate(edir)
      deallocate(ecan)
      deallocate(etrans)
      deallocate(esnow)
      deallocate(avgedir)
      deallocate(avgecan)
      deallocate(avgetrans)
      deallocate(avgesnow)
      deallocate(avgpotevp)
      deallocate(ti)
! GSD
      deallocate(rainc_bucket)
      deallocate(rainnc_bucket)
      deallocate(pcp_bucket)
      deallocate(snow_bucket)
      deallocate(graup_bucket)
      deallocate(qrmax)
      deallocate(tmax)
      deallocate(snownc)
      deallocate(graupelnc)
! SRD
      if (.not. minimal_memory) then
        if (allocated(wspd10max)) deallocate(wspd10max)
        if (allocated(w_up_max)) deallocate(w_up_max)
        if (allocated(w_dn_max)) deallocate(w_dn_max)
        if (allocated(w_mean)) deallocate(w_mean)
        if (allocated(refd_max)) deallocate(refd_max)
        if (allocated(prate_max)) deallocate(prate_max)
        if (allocated(fprate_max)) deallocate(fprate_max)
        if (allocated(up_heli_max)) deallocate(up_heli_max)
        if (allocated(up_heli_max16)) deallocate(up_heli_max16)
        if (allocated(up_heli_min)) deallocate(up_heli_min)
        if (allocated(up_heli_min16)) deallocate(up_heli_min16)
        if (allocated(up_heli_max02)) deallocate(up_heli_max02)
        if (allocated(up_heli_min02)) deallocate(up_heli_min02)
        if (allocated(up_heli_max03)) deallocate(up_heli_max03)
        if (allocated(up_heli_min03)) deallocate(up_heli_min03)
        if (allocated(rel_vort_max)) deallocate(rel_vort_max)
        if (allocated(rel_vort_max01)) deallocate(rel_vort_max01)
        if (allocated(rel_vort_maxhy1)) deallocate(rel_vort_maxhy1)
        if (allocated(wspd10umax)) deallocate(wspd10umax)
        if (allocated(wspd10vmax)) deallocate(wspd10vmax)
        if (allocated(refdm10c_max)) deallocate(refdm10c_max)
        if (allocated(hail_max2d)) deallocate(hail_max2d)
        if (allocated(hail_maxk1)) deallocate(hail_maxk1)
        if (allocated(grpl_max)) deallocate(grpl_max)
        if (allocated(up_heli)) deallocate(up_heli)
        if (allocated(up_heli16)) deallocate(up_heli16)
        if (allocated(ltg1_max)) deallocate(ltg1_max)
        if (allocated(ltg2_max)) deallocate(ltg2_max)
        if (allocated(ltg3_max)) deallocate(ltg3_max)
        if (allocated(nci_ltg)) deallocate(nci_ltg)
        if (allocated(nca_ltg)) deallocate(nca_ltg)
        if (allocated(nci_wq)) deallocate(nci_wq)
        if (allocated(nca_wq)) deallocate(nca_wq)
        if (allocated(nci_refd)) deallocate(nci_refd)
        if (allocated(nca_refd)) deallocate(nca_refd)
      endif

! CRA
      if (.not. minimal_memory) then
        if (allocated(REF_10CM)) deallocate(REF_10CM)
        if (allocated(REF1KM_10CM)) deallocate(REF1KM_10CM)
        if (allocated(REF4KM_10CM)) deallocate(REF4KM_10CM)
      endif
      deallocate(REFC_10CM)
! CRA
      deallocate(U10mean)
      deallocate(V10mean)
      deallocate(SPDUV10mean)
      deallocate(SWRADmean)
      deallocate(SWNORMmean)
      deallocate(SNFDEN)
      deallocate(SNDEPAC)

!
!     FROM MASKS
!
      deallocate(hbm2)
      deallocate(sm)
      deallocate(sice)
      deallocate(lmh)  ! real
      deallocate(lmv)  ! real
      deallocate(gdlat)
      deallocate(gdlon)
      deallocate(dx)
      deallocate(dy)
      deallocate(htm)
      deallocate(vtm)

! add GFIP ICING
      if (.not. minimal_memory) then
        if (allocated(icing_gfip)) deallocate(icing_gfip)
        if (allocated(icing_gfis)) deallocate(icing_gfis)

! add GTG turbulence
        if (allocated(catedr)) deallocate(catedr)
        if (allocated(mwt)) deallocate(mwt)
        if (allocated(gtg)) deallocate(gtg)
      endif

!
      if (gocart_on) then
! Deallocate GOCART fields
! vrbls4d
        deallocate(dust)
        deallocate(salt)
        deallocate(soot)
        deallocate(waso)
        deallocate(suso)
        deallocate(pp25)
        deallocate(pp10)
! vrbls3d
        deallocate(ext)
        deallocate(asy)
        deallocate(ssa)
        deallocate(sca)
        deallocate(duem)
        deallocate(dusd)
        deallocate(dudp)
        deallocate(duwt)
        deallocate(dusv)
        deallocate(suem)
        deallocate(susd)
        deallocate(sudp)
        deallocate(suwt)
        deallocate(ocem)
        deallocate(ocsd)
        deallocate(ocdp)
        deallocate(ocwt)
        deallocate(ocsv)
        deallocate(bcem)
        deallocate(bcsd)
        deallocate(bcdp)
        deallocate(bcwt)
        deallocate(bcsv)
        deallocate(ssem)
        deallocate(sssd)
        deallocate(ssdp)
        deallocate(sswt)
        deallocate(sssv)
        deallocate(dpres)
        deallocate(rhomid)
! vrbls2d
        deallocate(dusmass)
        deallocate(ducmass)
        deallocate(dusmass25)
        deallocate(ducmass25)
        deallocate(susmass)
        deallocate(sucmass)
        deallocate(susmass25)
        deallocate(sucmass25)
        deallocate(ocsmass)
        deallocate(occmass)
        deallocate(ocsmass25)
        deallocate(occmass25)
        deallocate(bcsmass)
        deallocate(bccmass)
        deallocate(bcsmass25)
        deallocate(bccmass25)
        deallocate(sssmass)
        deallocate(sscmass)
        deallocate(sssmass25)
        deallocate(sscmass25)
        deallocate(dustcb)
        deallocate(occb)
        deallocate(bccb)
        deallocate(sulfcb)
        deallocate(pp25cb)
        deallocate(pp10cb)
        deallocate(sscb)
        deallocate(dustallcb)
        deallocate(ssallcb)
        deallocate(dustpm)
        deallocate(sspm)
      endif
!
! HWRF RRTMG output 
      deallocate(acswupt)
      deallocate(swdnt)
      deallocate(acswdnt)
      
      end
