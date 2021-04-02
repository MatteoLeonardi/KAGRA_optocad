Program LAYOUT
!  Optical layout for Advanced VIRGO.
!  Julien Marque, Mirko Prijatelj, Loic Rolland, Romain Bonnand   30 Sept 2013

    use optocad
    use rsplot
    use rsutil
    implicit none
      
    integer, parameter       :: nt=10             ! number of tanks (main Virgo towers: SIB1, PR, IMC, BS, NI, WI, NE, WE, SR, SDB1)
    integer, parameter       :: nmt=7             ! number of benches suspended in mini-towers (SIB2, SDB2, SNEB, SWEB, SPRB, SQB, CCB)
    integer, parameter       :: neb=16            ! number of external benches
    integer, parameter       :: mt=nt+nmt+neb     ! total number of tanks + benches in mini-towers + external benches
    integer                  :: ibf=-1,ib2f=-1,cf=-1,wf=-1,nf=-1,mcf=-1,wbf=-1,prbf=-1,prmf=-1,eib2f=-1,mcemf=-1,fake=-1 ! reference numbers of frames
    integer                  :: nimf=-1, wimf=-1,nemf=-1,wemf=-1                                                         ! reference numbers of frames
    integer                  :: dbf=-1,db2f=-1,nbf=-1,bsf=-1,eibf=-1,lbf=-1,af=-1,rcf=-1,ercf=-1,emcbf=-1,db3f=-1,db4f=-1 ! reference numbers of frames
    integer                  :: db5f=-1, db6f=-1, esqb1f=-1, esqb2f=-1 ! reference numbers of frames
    integer                  :: tempf=-1 ! reference numbers of frames   
    integer                  :: dlinkf=-1,ebf=-1  ! reference numbers of frames
    integer                  :: lcg,ltg,lcben,lcminitower,it,i,ibs=0
    integer                  :: lcbc1, lcbc2, lcbc3, ltb, lccs, nib, nib2, ib
    integer                  :: lcbni, lcb0, ltb0, lcb1, lcb2, lcb3
    integer                  :: lcbaux0, ltbaux0, lcbaux1, lcbaux2, lcbaux3, lcbauxc1, lcbauxc2,lcbauxc3,ltbaux
    integer                  :: icc, icpd, iccam, icqpd, icpsd

    character(128)           :: omc(9), omc2(9), pd(6), pd2(6), cam(6), cam2(6), qpd(6), galvo(12), standa8(6), standa6(8)
    character(128)           :: minitower1(21), minitower2(20), minitower3(12), u200(12), m8822(12), u100(12), qpd2(6), FarIsol(12)
    character                :: ocd*32, version*16
    character(40)            :: dwi(5)
!    character(100),parameter :: printPar=''
!    character(100),parameter :: printPar='rs act w2t z2t lb'     !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb'

!   character(200),parameter :: printPar='rs s2 act rd ang w0t w2t z0t z2t pw lb'     !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb'
!  character(100),parameter :: printPar='rs act ang an2 pw lb'     !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb'
!   character(100),parameter :: printPar='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6'     !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb'

    
    
    real                     :: arcrange 
    real                     :: tank(2,mt), trad                         !positions of the tank and benches, and tank radius
    real                     :: minitower_l1, minitower_l2
    real                     :: cross(2,5), pst(6)   
    real                     :: psc, psc2, pzc, pzc2, pzc3, pzc4, pez, pez2    !frame scaling factors
    real                     :: pof, iof                                 !offset on page and offset between frames
    real                     :: cfxs, cfxe, cfys, cfye, cfxrange, cfyrange,cfxof, cfyof           !parameters for central building frame
    real                     :: wfxs, wfxe, wfys, wfye, wfxrange, wfyrange,wfxof, wfyof           !parameters for WE frame
    real                     :: nfxs, nfxe, nfys, nfye, nfxrange, nfyrange,nfxof, nfyof           !parameters for NE frame
    real                     :: afxs, afxe, afys, afye, afxrange, afyrange,afxof, afyof           !parameters for annotations frame
    real                     :: mcfxs, mcfxe, mcfys, mcfye, mcfxrange,mcfyrange, mcfxof, mcfyof   !parameters for MC frame
    real                     :: ibzxs, ibzxe, ibzys, ibzye, ibzxrange,ibzyrange, ibzxof, ibzyof   !parameters for SIB1 frame
    real                     :: ib2zxs, ib2zxe, ib2zys, ib2zye, ib2zxrange,ib2zyrange, ib2zxof, ib2zyof   !parameters for SIB2 frame
    real                     :: bszxs, bszxe, bszys, bszye, bszxrange,bszyrange, bszxof, bszyof   !parameters for BS frame
    real                     :: dbxs, dbxe, dbys, dbye, dbxrange,dbyrange, dbxof, dbyof, dlinkxof, dlinkyof   !parameters for SDB1 frame
    real                     :: db2zxs, db2zxe, db2zys, db2zye, db2zxrange,db2zyrange, db2zxof, db2zyof   !parameters for SDB2 frame
    real                     :: db3zxs, db3zxe, db3zys, db3zye, db3zxrange,db3zyrange, db3zxof, db3zyof   !parameters for SQB frame (squeez bench)
    real                     :: db4zxs, db4zxe, db4zys, db4zye, db4zxrange,db4zyrange, db4zxof, db4zyof   !parameters for CCB frame (control cavity bench)
    real                     :: db5zxs, db5zxe, db5zys, db5zye, db5zxrange,db5zyrange, db5zxof, db5zyof   !parameters for IMFC frame (Input Mirror Filter Cavity)
    real                     :: db6zxs, db6zxe, db6zys, db6zye, db6zxrange,db6zyrange, db6zxof, db6zyof   !parameters for EMFC frame (End Mirror Filter Cavity)
    real                     :: esqb1zxs, esqb1zxe, esqb1zys, esqb1zye, esqb1zxrange,esqb1zyrange, esqb1zxof, esqb1zyof    !parameters for ESQB1 frame (External Squeezing Bench 1)
    real                     :: esqb2zxs, esqb2zxe, esqb2zys, esqb2zye, esqb2zxrange,esqb2zyrange, esqb2zxof, esqb2zyof    !parameters for ESQB2 frame (External Squeezing Bench 1)
    real                     :: ebzxs, ebzxe, ebzys, ebzye, ebzxrange,ebzyrange, ebzxof, ebzyof   !parameters for EDB frame
    real                     :: nbzxs, nbzxe, nbzys, nbzye, nbzxrange,nbzyrange, nbzxof, nbzyof, nbwidth   !parameters for SNEB frame
    real                     :: wbzxs, wbzxe, wbzys, wbzye, wbzxrange,wbzyrange, wbzxof, wbzyof, wbwidth !parameters for SWEB frame
    real                     :: prbzxs, prbzxe, prbzys, prbzye, prbzxrange,prbzyrange, prbzxof, prbzyof, prbwidth, prbrot   !parameters for SPRB frame
    real                     :: prmzxs, prmzxe, prmzys, prmzye, prmzxrange,prmzyrange, prmzxof, prmzyof, prmwidth  !parameters for PR mirror frame
    real                     :: nemzxs, nemzxe, nemzys, nemzye, nemzxrange,nemzyrange, nemzxof, nemzyof, nemwidth  !parameters for NE mirror frame
    real                     :: wemzxs, wemzxe, wemzys, wemzye, wemzxrange,wemzyrange, wemzxof, wemzyof, wemwidth  !parameters for WE mirror frame
    real                     :: nimzxs, nimzxe, nimzys, nimzye, nimzxrange,nimzyrange, nimzxof, nimzyof, nimwidth  !parameters for NI mirror frame
    real                     :: wimzxs, wimzxe, wimzys, wimzye, wimzxrange,wimzyrange, wimzxof, wimzyof, wimwidth  !parameters for WI mirror frame
    real                     :: eibzxs, eibzxe, eibzys, eibzye, eibzxrange,eibzyrange, eibzxof, eibzyof
    real                     :: eabzxs, eabzxe, eabzys, eabzye, eabzxrange,eabzyrange, eabzxof, eabzyof
    real                     :: eib2zxs, eib2zxe, eib2zys, eib2zye,eib2zxrange, eib2zyrange, eib2zxof, eib2zyof
    real                     :: lbzxs, lbzxe, lbzys, lbzye, lbzxrange,lbzyrange, lbzxof, lbzyof    !parameters for Laser Bench frame
    real                     :: rczxs, rczxe, rczys, rczye, rczxrange,rczyrange, rczxof, rczyof
    real                     :: erczxs, erczxe, erczys, erczye, erczxrange,erczyrange, erczxof, erczyof
    real                     :: mczxs, mczxe, mczys, mczye, mczxrange,mczyrange, mczxof, mczyof
    real                     :: emcbzxs, emcbzxe, emcbzys, emcbzye, emcbzxrange,emcbzyrange, emcbzxof, emcbzyof
    real                     :: xb(13), yb(13), rfci(13), rfci2
    real                     :: xsdb1(9), ysdb1(9) 
    integer                  :: npointsSDB1 
    real                     :: ref0(2)
    real                     :: sdb_y_tuning, hws_tuning
    real                     :: inch
    
    integer                  :: beamMain, beamHartmannDET, beamHartmannINJ, beamAuxSNEB, beamAuxSWEB, beamOLeverSNEB, beamOLeverSWEB !associate a readable variable to the beam number. The associated number should be in the order of their appearance in ocd file.
    integer                  :: beamGreenSQZ     
 
    !Variables to optimize the SDB1 MMT lenses !LR
    integer                  :: kk,ii,nn, m, beamId, cavityId
    integer,parameter        :: ndL      = 3      !number of distance between lenses in the scan
    integer,parameter        :: nRocLtwo = 10      !number of lens L2 Roc in the scan for SDB1 MMT L2 telescope
    integer,parameter        :: nRocLthr = 10      !number of lens L3 Roc in the scan for SDB1 MMT L3 telescope
    integer,parameter        :: ncl = 100          !number of colors (z-scale) for contour plots
    real                     :: moma( 1:nRocLthr,1:nRocLtwo)
    real                     :: waist(1:nRocLthr,1:nRocLtwo)
    real                     :: wsize(1:nRocLthr,1:nRocLtwo)
    real                     :: curv( 1:nRocLthr,1:nRocLtwo)
    real                     :: zdist(1:nRocLthr,1:nRocLtwo)
    real                     :: SDB1_L2_Roc(1:nRocLtwo), SDB1_L3_Roc(1:nRocLthr), rcl(ncl), SDB1_deltaL(ndL)
    real                     :: OMC_waist,OMC_range_deltaWaist,OMC_range_deltaDist    
    !Variables to define the OMC
!    real,parameter           :: OMC_xB=-0.006465,OMC_yB=0.03041,OMC_rd=0.0065006
!    real,parameter           :: OMC_WL=0.05943,OMC_EH=0.05946,OMC_Rc=0.780
!    real,parameter           :: OMC_width=0.025860, OMC_alpha=6.0

      ocd=arg(1,'tmp_AdV_Layout.ocd') ! get name of input file
                                !      ocd=arg(1,'tmp_'//trim(arg(0))//'.ocd') ! get name of input file
                                !version=ocd(scan(ocd,'_')+1:index(ocd,'.ocd')-1) ! extract version
      version='93g'
      
      inch = 2.54e-2            !size of 1'', in meters
      call oc_bind('inch',inch)
    
! Suspended Injection & Detection Bench shape
    yb(:)=(/.44,.44,.336,.182,-.182,-.336,-.44,-.44,-.336,-.182,.182,.336,.44/)
    xb(:)=(/.182,-.182,-.336,-.44,-.44,-.336,-.182,.182,.336,.44,.44,.336,.182/)
    rfci=-100. ! vector shift to plot SIB (below)
    rfci2=-100.! shift to plot SIB (below)
    npointsSDB1 = 8+1                                                
    ysdb1(:)=(/.44,  .44,  .182, -.182,-.44, -.44, -.182,.182,.44/)  
    xsdb1(:)=(/.182,-.182,-.44,  -.44, -.182, .182, .44, .44, .182/) 

!  The following data are measured in the CBRS (with x=-Wc)

!  Tanks:              xpos       ypos
    tank(:,  1)=(/    -0.027,  -11.000/) ! SIB1
    tank(:,  2)=(/  -143.699,  -11.064/) ! IMC end Mirror
    tank(:,  3)=(/    -0.017,   -6.085/) ! PRM
    tank(:,  4)=(/     0.003,    0.023/) ! BS
    tank(:,  5)=(/     0.000,    5.774/) ! IMX
    tank(:,  6)=(/     0.000, 3005.774/) ! EMX
    tank(:,  7)=(/    -5.600,    0.000/) ! IMY
    tank(:,  8)=(/ -3005.600,    0.000/) ! EMY
    tank(:,  9)=(/     6.044,    0.018/) ! SRM
    tank(:, 10)=(/    10.971,   -0.002/) ! SDB1
    trad=1.0                             ! radius of the tanks

! Benches in minitowers and external
    tank(:,11)=(/            -1.816,          -15.32500/) ! EIB (external)
    tank(:,12)=(/  tank(1,10)+3.143,    tank(2,10)     /) ! SDB2 (minitower)
    tank(:,13)=(/  tank(1, 8)-7.730,    tank(2,8)-0.120/) ! SWEB center (minitower)
    tank(:,14)=(/  tank(1, 6)-0.120,    tank(2,6)+7.730+0.626/) ! SNEB center  (minitower)
!    tank(:,15)=(/            -4.576,           -14.925/) ! LB  (external)
!    tank(:,15)=(/            -5.056,           -15.3250/) ! LB  (external)
    tank(:,15)=(/            -5.056+0.44+0.30, -15.3250/) ! LB  (external new)
    tank(:,16)=(/    tank(1,1)-0.42,    tank(2,1)-1.615/) ! EIB2 (external)
    tank(:,17)=(/    tank(1,1) -100,    tank(2,1) -100 /) ! SIB1-below (SIB1 tank)
    tank(:,18)=(/    tank(1,17)-0.96,   tank(2,17)-2.10/) ! ERFCB external reference cavity bench (external)
    tank(:,19)=(/    tank(1,2) -2.00,   tank(2,2) -0.30/) ! EMCB  external mode cleaner bench (external)
    tank(:,20)=(/    tank(1,12)+1.832,   tank(2,12)-0.098/) ! EDB2 (external)
    tank(:,21)=(/    tank(1,13)-0.87,   tank(2,13)+0.00/) ! EWEB1 (external)
    tank(:,22)=(/    tank(1,13)+1.70,   tank(2,13)-0.80/) ! EWEB2 (external)
    tank(:,23)=(/    tank(1,14)+0.00,   tank(2,14)+0.87/) ! ENEB1 (external)
    tank(:,24)=(/    tank(1,14)-0.80,   tank(2,14)-1.70/) ! ENEB2 (external)
    tank(:,25)=(/          -1.111037,             -2.91395/) ! SPRB center (minitower) (ok within 1mm with mechanics CAO, B. Lieunard,mail from July 15th 2013).
    tank(:,26)=(/    tank(1,1)+3.085,   tank(2,1)/)       ! SIB2 center (minitower)
    tank(:,27)=(/            -0.877,          -13.695/) ! EAB (external)   
    tank(:,28)=(/    tank(1,10)+0.300,   tank(2,10)+3.310 /) ! SQB1 (squeezing minitower) 
    tank(:,29)=(/    tank(1,28)-8.700, tank(2,28)+7.1275 /) ! SQB2 (squeezing microtower)
    tank(:,30)=(/    tank(1,29)-0.1, tank(2,29)+51.7-0.0043-3.31-7.1275 /) ! IMFC (Input Mirror Filter Cavity)
    tank(:,31)=(/    tank(1,30),   tank(2,30)+300.0 /) ! EMFC (End Mirror Filter Cavity)
    tank(:,32)=(/    tank(1,28)+0.850,  tank(2,28)+1.226 /) ! ESQB (External Squeezing Bench 1)
    


!Some fine-tuning of bench alignments
    sdb_y_tuning = +140e-6  ! y-offset of SDB1 and SDB2 wrt nominal position, in meters


! Mini-tower dimensions
    minitower_l1 = 1.380  !from door to door
    minitower_l2 = 1.384  !from viewport to viewport
    ! array used to set reference point of mini-towers to their center
    ref0(:)=(/.5,.5/)

!  Set up data for demo waist indicators:
!            act      xap      yap     rap  phi    cur
    dwi(1)='#o 12.,  -15.'                                 ! origin
    dwi(2)='b     105.920, 103.340,.033648, 180.,4.551'    ! tang. plane
    dwi(3)='c  d, 105.520, 103.340, .00001'                ! beam stop
    dwi(4)='b     105.520, 103.040,.033648,   0.,4.551'    ! sagi. plane
    dwi(5)='c  d, 105.920, 103.040, .00001'                ! beam stop



psc=0.024      ! Scaling factor.    WARNING: it was initially 0.025. Modified such that SNEB and SWEB are in the layout (LR)
psc2=1000.*psc
pzc=0.25
pzc2=1000.*pzc
pzc3=0.22
pzc4=0.13 
pez=0.125
pez2=1000.*pez
pof=30.        ! offset on page
iof=25.        ! offset between frames

!! Info for the frame for the 'Annotations' (Title, authors, ...)
afxof=850.
afyof=725.
afxs=100.
afxe=111.
afys=102.5
afye=106.5

!##############################################################################
! Beam numbering (-1 means not yet defined in ocd file)
beamMain        =  1
beamHartmannDET =  2
beamHartmannINJ = -1 
beamGreenSQZ    =  3
beamAuxSNEB     = -1 
beamAuxSWEB     = -1 
beamOLeverSNEB  = -1 
beamOLeverSWEB  = -1 



!##############################################################################
! Global Layout parameters

!! Info for the frame of the central building in the Global Drawing
cfxs = -12.           !coordinates of the x-lower corner of the general central frame cf (m)
cfxe =  17.           !coordinates of the x-upper corner of the general central frame cf (m)
cfys = -16.           !coordinates of the y-lower corner of the general central frame cf (m)
cfye =  12.           !coordinates of the y-upper corner of the general central frame cf (m)
cfxrange=cfxe-cfxs    !range of the general central frame along x (m)                    
cfyrange=cfye-cfys    !range of the general central frame along y (m)                    

!! Info for the frame of the west station in the Global Drawing
wfxs = -3014.4        !coordinates of the x-lower corner of the general west frame wf (m)
wfxe = -2999.4        !coordinates of the x-upper corner of the general west frame wf (m)
wfys = -2.            !coordinates of the y-lower corner of the general west frame wf (m)
wfye = +2.            !coordinates of the y-upper corner of the general west frame wf (m)
wfxrange=wfxe-wfxs    !range of the general west frame along x (m)                       
wfyrange=wfye-wfys    !range of the general west frame along y (m)                       

!! Info for the frame of the north station in the Global Drawing
nfxs = -2.            !coordinates of the x-lower corner of the general north frame nf (m)
nfxe = +2.            !coordinates of the x-upper corner of the general north frame nf (m)
nfys =  3000.         !coordinates of the y-lower corner of the general north frame nf (m)
nfye =  3015.         !coordinates of the y-upper corner of the general north frame nf (m)
nfxrange=nfxe-nfxs    !range of the general north frame along x (m)                      
nfyrange=nfye-nfys    !range of the general north frame along y (m)                      

!! Info for the frame of the Mode Cleaner mirror in the Global Layout
mcfxs=-148.
mcfxe=-133.
mcfys=-14.
mcfye=-8.
mcfxof=pof
mcfyof=pof+50

!! Positions of the frames in the Global Layout
cfxof=wfxrange*psc2+iof+pof
cfyof=pof
wfxof=pof
wfyof=pof-(cfys+wfyrange/2.)*psc2
nfxof=pof+100
nfyof=wfyof+wfyrange*psc2+iof

!##############################################################################
! Input Benches Layout parameters

!LB
lbzxof=pof
lbzyof=pof
lbzxs=tank(1,15)-0.02
lbzxe=tank(1,15)+2.42
!lbzxs=tank(1,15)+0.86 !Moved as far right as possible
!lbzxe=tank(1,15)+2.86 !Moved as far right as possible
lbzys=tank(2,15)-0.02
!lbzys=tank(2,15)+0.31
lbzye=tank(2,15)+1.31
lbzxrange=lbzxe-lbzxs
lbzyrange=lbzye-lbzys

!EAB
!eabzxof=pof+133
eabzxof=pof+150
eabzyof=pof+375. 
eabzxs=tank(1,27)-0.02
eabzxe=tank(1,27)+0.47
eabzys=tank(2,27)-0.02
eabzye=tank(2,27)+0.47
eabzxrange=eabzxe-eabzxs
eabzyrange=eabzye-eabzys

!EIB1
eibzxof=pof
eibzyof=pof 
eibzxs=tank(1,11)-0.02
eibzxe=tank(1,11)+2.42
eibzys=tank(2,11)-0.02
eibzye=tank(2,11)+1.62
eibzxrange=eibzxe-eibzxs
eibzyrange=eibzye-eibzys

!EIB2
eib2zxof=320
eib2zyof=455.
eib2zxs=-.5-0.025
eib2zxe=.5-0.025
eib2zys=tank(2,16)-0.1
eib2zye=tank(2,16)+.21
eib2zxrange=eib2zxe-eib2zxs
eib2zyrange=eib2zye-eib2zys

!SIB1 top
ibzxof=320.
ibzyof=545.
ibzxs=-.5-0.025
ibzxe=.5-0.025
ibzys=-11.5+.05
ibzye=-10.6+.15
ibzxrange=ibzxe-ibzxs
ibzyrange=ibzye-ibzys

!SIB1 below
rczxof=775.
rczyof=200.
rczxs=ibzxs-100
rczxe=ibzxe-100
rczys=ibzys-100
rczye=ibzye-100-0.1
rczxrange=rczxe-rczxs
rczyrange=rczye-rczys

!ERCB
erczxof=670.
erczyof=pof
erczxs=tank(1,18)-0.02
erczxe=tank(1,18)+1.22
erczys=tank(2,18)-0.07
erczye=tank(2,18)+0.63
erczxrange=erczxe-erczxs
erczyrange=erczye-erczys

!SIB2
ib2zxof=700.
ib2zyof=415.
ib2zxs=2.35
ib2zxe=4.15
ib2zys=-11.70
ib2zye=-10.30
ib2zxrange=ib2zxe-ib2zxs
ib2zyrange=ib2zye-ib2zys

!MC Mirror
mczxof=155.
mczyof=575.
mczxs=tank(1,2)-0.25
mczxe=tank(1,2)+0.25
mczys=tank(2,2)-0.25
mczye=tank(2,2)+0.25
mczxrange=mczxe-mczxs
mczyrange=mczye-mczys

!EMCB
emcbzxof=pof
emcbzyof=547.
emcbzxs=tank(1,2)-2.02
emcbzxe=tank(1,2)-1.68
emcbzys=tank(2,2)-0.37
emcbzye=tank(2,2)+0.32
emcbzxrange=emcbzxe-emcbzxs
emcbzyrange=emcbzye-emcbzys

!##############################################################################
! Output Benches Layout parameters

!Info for Suspended NE Bench (SNEB) frame in the "Zoom Frames on BS and Output Benches" Layout
nbwidth=1.3                          !width of the bench (m)
nbzxof=600.                          !x position of the frame on the page
nbzyof=350.                          !y position of the frame on the page
nbzxs=tank(1,14)-nbwidth/2-0.70      !coordinates of lower corner of the frame along x (m)
nbzxe=tank(1,14)+nbwidth/2+0.10      !coordinates of upper corner of the frame along x (m)
nbzys=tank(2,14)-nbwidth/2-1.90      !coordinates of lower corner of the frame along y (m)
nbzye=tank(2,14)+nbwidth/2+0.50      !coordinates of upper corner of the frame along y (m)

!Info for Suspended WE Bench (SWEB) frame in the "Zoom Frames on BS and Output Benches" Layout
wbwidth=1.3                          !width of the bench (m)   
wbzxof=pof                           !x position of the frame on the page
wbzyof=pof                           !y position of the frame on the page
wbzxs=tank(1,13)-nbwidth/2-0.50      !coordinates of lower corner of the frame along x (m)
wbzxe=tank(1,13)+nbwidth/2+1.90      !coordinates of upper corner of the frame along x (m)
wbzys=tank(2,13)-nbwidth/2-0.70      !coordinates of lower corner of the frame along y (m)
wbzye=tank(2,13)+nbwidth/2+0.10      !coordinates of upper corner of the frame along y (m)

! Info for BS mirror frame in the "Zoom Frames on BS and Output Benches" Layout
bszxof= nbzxof !same start as NE frame
bszyof= wbzyof !same start as WE frame
bszxs = nbzxs
bszxe = nbzxe
bszys = wbzys
bszye = wbzye


! Info for SDB1 frame in the "Zoom Frames Dark Fringe Benches" Layout
dbxof=30. 
dbyof=123. 
dlinkxof = 30
dlinkyof = 550
dbxs=tank(1,10)-.5
dbxe=tank(1,10)+.5
dbys=tank(2,10)-.5
dbye=tank(2,10)+.5
dbxrange=dbxe-dbxs
dbyrange=dbye-dbys

! Info for SDB2 frame in the "Zoom Frames Dark Fringe Benches" Layout
db2zxof=550. 
db2zyof=40. 
db2zxs=tank(1,12)-.75
db2zxe=tank(1,12)+.75
db2zys=tank(2,12)-.70  !to be aligned with SDB1 frame (0.21 offset between bench centers, and frame of 0.5 around SDB1 center)
db2zye=tank(2,12)+.70
db2zxrange=db2zxe-db2zxs
db2zyrange=db2zye-db2zys

! Info for SQB frame in the "Zoom Frames Squeezing Benches" Layout
db3zxof=0. 
db3zyof=0. 
db3zxs=tank(1,28)-.70
db3zxe=tank(1,28)+2.950
db3zys=tank(2,28)-.70  
db3zye=tank(2,28)+1.651
db3zxrange=db2zxe-db2zxs
db3zyrange=db2zye-db2zys

! Info for CCB frame in the "Zoom Frames Squeezing Benches" Layout
db4zxof=0. 
db4zyof=0. 
db4zxs=tank(1,29)-.60
db4zxe=tank(1,29)+.60
db4zys=tank(2,29)-.875  
db4zye=tank(2,29)+.725
db4zxrange=db2zxe-db2zxs
db4zyrange=db2zye-db2zys

! Info for IMFC frame in the "Zoom Frames Squeezing Benches" Layout
db5zxof=0. 
db5zyof=0. 
db5zxs=tank(1,30)-0.40
db5zxe=tank(1,30)+0.40
db5zys=tank(2,30)-0.40  
db5zye=tank(2,30)+0.40
db5zxrange=db2zxe-db2zxs
db5zyrange=db2zye-db2zys

! Info for EMFC frame in the "Zoom Frames Squeezing Benches" Layout
db6zxof=0. 
db6zyof=0. 
db6zxs=tank(1,31)-.40
db6zxe=tank(1,31)+.40
db6zys=tank(2,31)-.40  
db6zye=tank(2,31)+.40
db6zxrange=db2zxe-db2zxs
db6zyrange=db2zye-db2zys

! Info for ESQB1 bench frame in the "Zoom Frames on Squeezing Benches" Layout
esqb1zxof=0.
esqb1zyof=0.
esqb1zxs=tank(1,32)-0.8
esqb1zxe=tank(1,32)+2.1
esqb1zys=tank(2,32)-0.850
esqb1zye=tank(2,32)+0.425
esqb1zxrange=ebzxe-ebzxs
esqb1zyrange=ebzye-ebzys

! Info for ESQB2 bench frame in the "Zoom Frames on Squeezing Benches" Layout
esqb2zxof=0.
esqb2zyof=0.
esqb2zxs=tank(1,32)-1.3
esqb2zxe=tank(1,32)+1.3
esqb2zys=tank(2,32)-1.3
esqb2zye=tank(2,32)+1.3
esqb2zxrange=ebzxe-ebzxs
esqb2zyrange=ebzye-ebzys

! Info for EDB2 bench frame in the "Zoom Frames on Dark Fringe Benches" Layout
ebzxof=600.
ebzyof=400.
ebzxs=tank(1,20)-1.1
ebzxe=tank(1,20)+1.1
ebzys=tank(2,20)-0.5
ebzye=tank(2,20)+0.5
ebzxrange=ebzxe-ebzxs
ebzyrange=ebzye-ebzys

! Info for SPRB frame in the "Zoom on PR, BS and END benches" Layout
prbwidth=1.3                          !width of the bench (m)   
prbzxof=pof                           !x position of the frame on the page
prbzyof=550                           !y position of the frame on the page
prbzxs=tank(1,25)-nbwidth/2-0.50      !coordinates of lower corner of the frame along x (m)
prbzxe=tank(1,25)+nbwidth/2+0.90      !coordinates of upper corner of the frame along x (m)
prbzys=tank(2,25)-nbwidth/2-0.30      !coordinates of lower corner of the frame along y (m)
prbzye=tank(2,25)+nbwidth/2+0.50      !coordinates of upper corner of the frame along y (m)
!prbzys=tank(2,25)-nbwidth/2-0.20-3      !coordinates of lower corner of the frame along y (m)
!prbzye=tank(2,25)+nbwidth/2+0.70      !coordinates of upper corner of the frame along y (m)
prbrot=12.*pi/180.                    !rotation of the bench to be parallel to the incoming beam (rad)

! Info for PR mirror and POP frame in the "Zoom on PR, BS and END benches" Layout
prmwidth=1.                           !width of the mirror (m)   
prmzxof=pof+210                       !x position of the frame on the page
prmzyof=390                           !y position of the frame on the page
prmzxs=tank(1,3)-prmwidth/2           !coordinates of lower corner of the frame along x (m)
prmzxe=tank(1,3)+prmwidth/2           !coordinates of upper corner of the frame along x (m)
prmzys=tank(2,3)-prmwidth/2           !coordinates of lower corner of the frame along y (m)
prmzye=tank(2,3)+prmwidth/2           !coordinates of upper corner of the frame along y (m)


! Info for NI frame in the "Zoom on PR, BS and END benches" Layout
nimwidth=1.                           !width of the mirror (m)   
nimzxof=pof+980                       !x position of the frame on the page
nimzyof=pof+300                       !y position of the frame on the page
nimzxs=tank(1,5)-nimwidth/2           !coordinates of lower corner of the frame along x (m)
nimzxe=tank(1,5)+nimwidth/2           !coordinates of upper corner of the frame along x (m)
nimzys=tank(2,5)-nimwidth             !coordinates of lower corner of the frame along y (m)
nimzye=tank(2,5)+nimwidth/4           !coordinates of upper corner of the frame along y (m)

! Info for NE frame in the "Zoom on PR, BS and END benches" Layout
nemwidth=1.0                          !width of the mirror (m)   
nemzxof=pof+980                       !x position of the frame on the page
nemzyof=pof+500                       !y position of the frame on the page
nemzxs=tank(1,6)-nemwidth/2           !coordinates of lower corner of the frame along x (m)
nemzxe=tank(1,6)+nemwidth/2           !coordinates of upper corner of the frame along x (m)
nemzys=tank(2,6)-nemwidth/4           !coordinates of lower corner of the frame along y (m)
nemzye=tank(2,6)+nemwidth/4           !coordinates of upper corner of the frame along y (m)

! Info for WI frame in the "Zoom on PR, BS and END benches" Layout
wimwidth=1.                           !width of the mirror (m)   
wimzxof=pof+980                       !x position of the frame on the page
wimzyof=pof                           !y position of the frame on the page
wimzxs=tank(1,7)-wimwidth/4           !coordinates of lower corner of the frame along x (m)
wimzxe=tank(1,7)+wimwidth             !coordinates of upper corner of the frame along x (m)
wimzys=tank(2,7)-wimwidth/2           !coordinates of lower corner of the frame along y (m)
wimzye=tank(2,7)+wimwidth/2           !coordinates of upper corner of the frame along y (m)

! Info for WE frame in the "Zoom on PR, BS and END benches" Layout
wemwidth=1.0                          !width of the mirror (m)   
wemzxof=pof+880                       !x position of the frame on the page
wemzyof=pof                           !y position of the frame on the page
wemzxs=tank(1,8)-wemwidth/4           !coordinates of lower corner of the frame along x (m)
wemzxe=tank(1,8)+wemwidth/4           !coordinates of upper corner of the frame along x (m)
wemzys=tank(2,8)-wemwidth/2           !coordinates of lower corner of the frame along y (m)
wemzye=tank(2,8)+wemwidth/2           !coordinates of upper corner of the frame along y (m)


!##############################################################################

!  Assign colors for optics, benches, grid lines:
    icc=ps_color((/.90,1.0,1.0/))      ! default color for interior of optics
    icpd=ps_color((/.0,.5,1./))        ! color for interior of ordinary photodiodes
    icqpd=10                           ! color for interior of quadrant photodiodes
    icpsd=14                           ! color for interior of position sensitive photodiodes
    iccam=5                            ! color for interior of CCD cameras
!    ic=ps_color((/.95,.80,1.0/),17)    ! color for cases of Faraday isolators
!    ic=ps_color((/.95,.80,.70/),18)    ! color for cases of Pockels cells
!    ic=ps_color((/.95,.95,.95/),19)    ! color mounting units and photodiodes

    lccs=7                             ! color of major surfaces of components
    lcminitower=8
    lcben=ps_color((/.0,.5,1./))       ! color for bench outlines
    lcbni=ps_color((/1.0,1.0,.85/))    ! color for bench interior
    lcg=ps_color((/.9,.9,.9/))         ! color for grid lines
    ltg=0                              ! line pattern for grid lines

!  Assign colors and line types for ray segments:
    lcb0=1
    ltb0=5
    lcb1=2                             ! color for beam at/upto 1*w
    lcb2=ps_color((/1.,.6,.6/))        ! color for beam at/upto 2*w
    lcb3=ps_color((/1.,.8,.8/))        ! color for beam at/upto 3*w
    lcbc1=ps_color((/1.,.7,.7/))       ! color for beam contour at 1*w
    lcbc2=lcb3                         ! color for beam contour at 2*w
    lcbc3=ps_color((/1.,.9,.9/))       ! color for beam contour at 3*w
    ltb=-1                             ! fill the beam area
    pst=(/100.,10.,1.,.1,.01,.001/)    ! Power thresholds for color steps
    
!  Assign colors and line types for ray segments (auxiliary beams):
    lcbaux0=1
    ltbaux0=5
    lcbaux1 =ps_color((/.0, .4, .4/))      ! color for beam at/upto 1*w
    lcbaux2 =ps_color((/.0, .6, .6/))      ! color for beam at/upto 2*w
    lcbaux3 =ps_color((/.0, .8, .8/))      ! color for beam at/upto 3*w
    lcbauxc1=ps_color((/.0, .7, .7/))      ! color for beam contour at 1*w
    lcbauxc2=lcbaux3                       ! color for beam contour at 2*w
    lcbauxc3=ps_color((/.0, .9, .9/))      ! color for beam contour at 3*w
    ltbaux=-1     

!  Bind some keywords (text strings) to color-index variables:
    call oc_bind('icc',icc)
    call oc_bind('icpd',icpd)
    call oc_bind('icqpd',icqpd)
    call oc_bind('icpsd',icpsd)
    call oc_bind('iccam',iccam)

!  Bind some keywords used to define the SDB1 MMT telescope parabolic mirrors
   call oc_bind('sdb1_mmt_xM1',0.360)       !X-position of center of HR face of mirror MMT_M1 (m) (in SDB1 referential)
   call oc_bind('sdb1_mmt_yM1',+0.01847634) !Y-position of center of HR face of mirror MMT_M1 (m) (in SDB1 referential)
   call oc_bind('sdb1_mmt_delta1',0.160)    !Offset of parabolic mirror MMT_M1 (m)
   call oc_bind('sdb1_mmt_delta2',0.010)    !Offset of parabolic mirror MMT_M2 (m)

!--------------------------------------   
!Nominal values for telescope SDB1
!--------------------------------------   
!   call oc_bind('sdb1_mmt_O1O2', 765.e-3) !Distance between the centers of the main parabola of the mirrors, O1 and O2 (m) (nominal 765e-3)
!   call oc_bind('sdb1_mmt_Roc1', 1.440)     !Roc of parabolic mirror MMT_M1 (m)  (nominal is 1.440 m)
!   call oc_bind('sdb1_mmt_Roc2',0.090)    !Roc of parabolic mirror MMT_M2 (m)  (nominal is 0.09000 m)
!--------------------------------------   
!Constructor values for telescope SDB1
!--------------------------------------   
   call oc_bind('sdb1_mmt_O1O2', 767.04e-3) !Distance between the centers of the main parabola of the mirrors, O1 and O2 (m) (nominal 765e-3)
   call oc_bind('sdb1_mmt_Roc1', 1.444)     !Roc of parabolic mirror MMT_M1 (m)  (nominal is 1.440 m)
   call oc_bind('sdb1_mmt_Roc2',0.09008)    !Roc of parabolic mirror MMT_M2 (m)  (nominal is 0.09000 m)
!--------------------------------------   

!  Bind some keywords used to define OMC MMT telescope lenses (and let possible some scan for optimisation)
   call oc_bind('SDB1_deltaL',-0.025)
   call oc_bind('SDB1_L2_Roc',0.247)
   call oc_bind('SDB1_L3_Roc',-0.340)
!   call oc_bind('OMC_xB',OMC_xB)
!   call oc_bind('OMC_yB',OMC_yB)
!   call oc_bind('OMC_rd',OMC_rd)
!   call oc_bind('OMC_WL',OMC_WL)
!   call oc_bind('OMC_EH',OMC_EH)
!   call oc_bind('OMC_Rc',OMC_Rc)
!   call oc_bind('OMC_width',OMC_width)
!   call oc_bind('OMC_alpha',OMC_alpha)

! Bind keyword for Hartmann Wavefront Sensor tuning.
   call oc_bind('hws_tuning',+0.000);
   
!  Bind the position of some tanks to use the position in the ocd configuration file, as reference coordinates
   call oc_bind('sdb1_x0',tank(1,10))
   call oc_bind('sdb1_y0',tank(2,10)+sdb_y_tuning)
   call oc_bind('sdb_minilinks_x0', tank(1,12))
   call oc_bind('sdb_minilinks_y0', tank(2,12))
   call oc_bind('sdb2_x0',tank(1,12))
   call oc_bind('sdb2_y0',tank(2,12)+sdb_y_tuning)
   call oc_bind('sdb2_minilinkLength',(tank(1,12)-0.8) - (tank(1,10)+1.5))
   call oc_bind('edb_x0',tank(1,20))
   call oc_bind('edb_y0',tank(2,20))
   call oc_bind('sneb_x0',tank(1,14))
   call oc_bind('sneb_y0',tank(2,14))
   call oc_bind('sweb_x0',tank(1,13))
   call oc_bind('sweb_y0',tank(2,13))
   call oc_bind('sprb_x0',tank(1,25))
   call oc_bind('sprb_y0',tank(2,25))
   call oc_bind('prbrot',prbrot*180./pi)
   call oc_bind('sqb1_x0',tank(1,28))
   call oc_bind('sqb1_y0',tank(2,28))
   call oc_bind('sqb2_x0',tank(1,29))
   call oc_bind('sqb2_y0',tank(2,29))

   
!! General definition of the OMC. Needed to put default values directly here: could not pass them through the component 'i' in ocd array
!! Needed to put alpha value directly in the OMC def (not in the 'p' line), because can not read the 'p' line if too long ??
   !!! omc(01)='p xB=-0.006465, yB=0.03041, rd=0.0065006, alpha=6.0, WL=0.05943, EH=0.05946, Rc=0.780, width=0.025860'
  !!!omc(01)='p xB=-0.006465, yB=0.03041, rd=0.0065006, WL=0.05943, EH=0.05946, Rc=0.780, width=0.025860'
  omc(01)='p rr=0.006552897,WL=0.059933,EH=0.059945,Rc=1.700,width=0.026086'
  omc(02)='c crtstr,-0.006517,0.0307125,rr,-84.,0.,r=0.9783,t=0.0217' 
  omc(03)=' +  str, rd=rr,    da=-2*6.0, rc=Rc,  r=0.999990,  t=0.000010'
  omc(04)=' +    d, rd=WL/2., da=-90.-6.0'
  omc(05)=' +  str, rd=rr,    da=-180,       c=0, r=0.9783, t=0.0217'
  omc(06)=' +  str, rd=rr,    da=-180-2*6.0, c=0, r=0.999990, t=0.000010'
  omc(07)=' +    d, rd=EH/2., da=-3*90.-6.0'
  omc(08)='d  d, -0.006517+width/2,0.0307125+0.003,rd=rr,ag=90-6.0,icc=1'
  omc(09)=' + d, dm=0.003'
! 
  omc2(01)='p rr=0.006552897,WL=0.059933,EH=0.059945,Rc=1.700,width=0.026086'
  omc2(02)='c crtstr,+0.006517,0.0307125,rr,-96.,0.,r=0.9783,t=0.0217'
  omc2(03)=' +   d, rd=EH/2.,da=-90.+6.0'
  omc2(04)=' + str, rd=rr,   da=-180+12.0,  c=0, r=0.999990, t=0.000010'
  omc2(05)=' + str, rd=rr,   da=-180,       c=0, r=0.9783, t=0.0217'
  omc2(06)=' +   d, rd=WL/2.,da=-3*90.+6.0'
  omc2(07)=' + str, rd=rr,   da=-360+2*6.0,rc=Rc,r=0.999990, t=0.000010'
  omc2(08)='d  d, +0.006517-width/2,0.0307125+0.003,rd=rr,ag=90+6.0,icc=1'
  omc2(09)=' + d, dm=0.003'

  call oc_bind('OMC',omc)
  call oc_bind('OMC2',omc2)

  FarIsol(01) = 'c,  t, 0.0, 0.0, 0.075, 0, icc=12, m=-1  '
  FarIsol(02) = '+   t, rd=0.160, da=-90'
  FarIsol(03) = '+   t, rd=0.075, da=-180'
  FarIsol(04) = '+   t, rd=0.160, da=-270'           
  FarIsol(05) = 'd,  t, 0.120, 0.0, 0.020, 0.0, m=0'
  FarIsol(06) = '+   t, dm=0.060'
  FarIsol(07) = ''
  FarIsol(08) = ''
  FarIsol(09) = ''
  FarIsol(10) = ''
  FarIsol(11) = ''
  FarIsol(12) = ''

  call oc_bind('FI',FarIsol);

!! Definition of Photodiode box view from top.
   pd(1) = 'd      d,    0,  0.005,    0.049,  0,   icc=18, m=-1'
   pd(2) = '+      d,dm=0.0341'
   pd(3) = 'h      t,    0,   0,  rd=0.006,   ag=0, m=-1 '
   pd(4) = '+      t, dm=0.034 '
   pd(5) = 'd      d,  0.0341, 0.005, rd=0.049, ag=0, icc=18, m=-1'
   pd(6) = '+      d, dm=0.0157 '

!! Definition of Photodiode box new design, the blue box represent approx. the size of the box.
!! Position to give represent the CCD.
   pd2(1) = 'd      d,    -0.0543,  0.0,    0.049,  0,   icc=18, m=-1'
   pd2(2) = '+      d,dm=0.0543'
   pd2(3) = 'h      t,    -0.05431,   0,  rd=0.0072,   ag=0, m=-1 '
   pd2(4) = '+      t, dm=0.0543 '
   pd2(5) = 'd      d,  0.0, 0.0, rd=0.049, ag=0, icc=18, m=-1'
   pd2(6) = '+      d, dm=0.0262 '

!! Definition of Newport mount 8822 for 2 inches optic with picomotors. 
!! Position to give is the optic center.
!   m8822(1) = 'd      d,    0.0,  0.0,    0.0373,  0,  icc=14, m=-1'
!   m8822(2) = '+      d,dm=0.001525'
!   m8822(3) = 'h      t,    0.0,   0,  rd=0.0254,   ag=0, m=-1 '
!   m8822(4) = '+      t, dm=0.001525'
!   m8822(5) = 'c  t, 0.00152500001, 0, rd=0.0373, ag=0, icc=14, m=-1'
!   m8822(6) = '+  t, rd=0.074542/2, da=-90'
!   m8822(7) = '+  t, rd=0.03575, da=-180'
!   m8822(8) = '+  t, rd=0.074542/2, da=-270'
!   m8822(9) = 'c  t, 0.019100001, 0, rd=0.0373, ag=0, icc=20, m=-1'
!   m8822(10) = '+  t, rd=0.0223/2, da=-90'
!   m8822(11) = '+  t, rd=0.03575, da=-180'
!   m8822(12) = '+  t, rd=0.0223/2, da=-270'

!   m8822(1) = 'd      d,    -0.0015,  0.0,    0.0373,  0,  icc=14, m=-1'
!   m8822(2) = '+      d,dm=0.0191'
!   m8822(3) = 'h      t,    -0.0015,   0,  rd=0.0254,   ag=0, m=-1 '
!   m8822(4) = '+      t, dm=0.0191'
!   m8822(5) = 'c  t, 0.024100000001, 0, rd=0.0373, ag=0, icc=14, m=-1'
!   m8822(6) = '+  t, rd=0.0526/2, da=-90'
!   m8822(7) = '+  t, rd=0.0373, da=-180'
!   m8822(8) = '+  t, rd=0.0526/2, da=-270'
!   m8822(9) = 'c  t, 0.0241, 0, rd=0.0373, ag=0, icc=20, m=-1'
!   m8822(10) = '+  t, rd=0.0159/2, da=-90'
!   m8822(11) = '+  t, rd=0.0373, da=-180'
!   m8822(12) = '+  t, rd=0.0159/2, da=-270'

m8822(1) = 'c   t,  0.0752, 0.0, rd=0.0746/2, ag=180, icc=14, m=-1'
m8822(2) = '+   t,  rd=0.0767/2, da=90'
m8822(3) = '+   d,  rd=0.0135/2, da=180'
m8822(4) = '+   d,  rd=0.0191/2, da=270'
m8822(5) = '+   t,  rd=0.0476/2, da=180'
m8822(6) = '+   d,  rd=0.0191/2, da=90'
m8822(7) = '+   d,  rd=0.0135/2, da=180'
m8822(8) = '+   t,  rd=0.0767/2, da=270'
  m8822(9) = 'c  t, 0.0399, 0, rd=0.0373, ag=0, icc=20, m=-1'
  m8822(10) = '+  t, rd=0.0353/2, da=-90'
  m8822(11) = '+  t, rd=0.0373, da=-180'
  m8822(12) = '+  t, rd=0.0353/2, da=-270'
!  m8822(13) = 'h, t, 0.0399, 0, rd=0.0373, ag=0, icc=20, m=-1'
!  m8822(14) = '+  t, dm=0.0353'


!! Definition of Newport mount U200 for 2 inches optic without picomotors. 
!! Position to give is the optic center.
!   u200(1) = 'd      d,    0.0,  0.0,    0.03575,  0,  icc=14, m=-1'
!   u200(2) = '+      d,dm=0.001525'
!   u200(3) = 'h      t,    0.0,   0,  rd=0.0254,   ag=0, m=-1 '
!   u200(4) = '+      t, dm=0.001525'
!   u200(5) = 'c  t, 0.00152500001, 0, rd=0.03575, ag=0, icc= 14, m=-1'
!   u200(6) = '+  t, rd=0.0585/2, da=-90'
!   u200(7) = '+  t, rd=0.03575, da=-180'
!   u200(8) = '+  t, rd=0.0585/2, da=-270'
!   u200(9) = 'c  t, 0.019100001, 0, rd=0.03575, ag=0, icc=20, m=-1'
!   u200(10) = '+  t, rd=0.0223/2, da=-90'
!   u200(11) = '+  t, rd=0.03575, da=-180'
!   u200(12) = '+  t, rd=0.0223/2, da=-270'

u100(1) = 'c   t,  0.0554, 0.0, rd=0.0254, ag=180, icc=14, m=-1'
u100(2) = '+   t,  rd=0.0569/2, da=90'
u100(3) = '+   d,  rd=0.0127/2, da=180'
u100(4) = '+   d,  rd=0.0185/2, da=270'
u100(5) = '+   t,  rd=0.0254/2, da=180'
u100(6) = '+   d,  rd=0.0185/2, da=90'
u100(7) = '+   d,  rd=0.0127/2, da=180'
u100(8) = '+   t,  rd=0.0569/2, da=270'
u100(9) = 'c  t, 0.033, 0, rd=0.0254, ag=0, icc=20, m=-1'
u100(10) = '+  t, rd=0.0224/2, da=-90'
u100(11) = '+  t, rd=0.0254, da=-180'
u100(12) = '+  t, rd=0.0224/2, da=-270'

u200(1) = 'c   t,  0.0561, 0.0, rd=0.0366, ag=180, icc=14, m=-1'
u200(2) = '+   t,  rd=0.0576/2, da=90'
u200(3) = '+   d,  rd=0.0128/2, da=180'
u200(4) = '+   d,  rd=0.0191/2, da=270'
u200(5) = '+   t,  rd=0.0476/2, da=180'
u200(6) = '+   d,  rd=0.0191/2, da=90'
u200(7) = '+   d,  rd=0.0128/2, da=180'
u200(8) = '+   t,  rd=0.0576/2, da=270'
u200(9) = 'c  t, 0.0369, 0, rd=0.0366, ag=0, icc=20, m=-1'
u200(10) = '+  t, rd=0.0192/2, da=-90'
u200(11) = '+  t, rd=0.0366, da=-180'
u200(12) = '+  t, rd=0.0192/2, da=-270'




!! Definition of Camera box view from top
   cam(1) = 'd  d, 0, 0, 0.049, 0, icc=5, m=-1'
   cam(2) = '+  d,dm=0.0341'
   cam(3) = 'h      t,    0,      0,  rd=0.006,   ag=0, m=-1 '
   cam(4) = '+      t, dm=0.034 '
   cam(5) = 'd      d,  0.0341, 0, rd=0.049, ag=0, icc=5, m=-1'
   cam(6) = '+      d, dm=0.0299 '

!! Definition of camera box new design, the yellow box represent approx. the size of the box.
!! Position to give represent the CCD.
   cam2(1) = 'd      d,    -0.087,  0.0,    0.045,  0,   icc=5, m=-1'
   cam2(2) = '+      d,dm=0.087'
   cam2(3) = 'h      t,    -0.087001,   0,  rd=0.006,   ag=0, m=-1 '
   cam2(4) = '+      t, dm=0.087 '
   cam2(5) = 'd      d,  0.0, 0.0, rd=0.045, ag=0, icc=5, m=-1'
   cam2(6) = '+      d, dm=0.025 '


!! Definition of Quadrant PhotoDiode box view from top (by NIKHEF)
   qpd(1) = 'd  d, 0, 0.00475, 0.05125, 0, icc=3, m=-1'
   qpd(2) = '+  d,dm=0.043001'
   qpd(3) = 'h      t,    0,      0,  rd=0.006,   ag=0, m=-1 '
   qpd(4) = '+      t, dm=0.043 '
   qpd(5) = 'd      d,  0.043001, 0.00475, rd=0.05125, ag=0, icc=3, m=-1'
   qpd(6) = '+      d, dm=0.1299 '

!! Definition of Quadrant PhotoDiode box view from top (by NIKHEF), position O
!correspond to sensor place, fixation is about 25 mm in front.
   qpd2(1) = 'd  d, -0.0455, 0.00, 0.05125, 0, icc=3, m=-1'
   qpd2(2) = '+  d,dm=0.0455'
   qpd2(3) = 'h      t,    -0.0455,      0,  rd=0.006,   ag=0, m=-1 '
   qpd2(4) = '+      t, dm=0.0454'
   qpd2(5) = 'd      d,  0.000, 0.000, rd=0.05125, ag=0, icc=3, m=-1'
   qpd2(6) = '+      d, dm=0.1274 '

!! Definition of commercial Standa mount for large optics 8 inches.
   standa8(1) = 'h  t, 0.000001, 0, 0.124, 0, m=-1'
   standa8(2) = '+  t, dm=0.039'
   standa8(3) = 'd  d, 0.039, 0.0, rd=0.124, ag=0, icc=1, m=-1'
   standa8(4) = '+  d, dm=0.090'
   standa8(5) = 'd  t, 0.129, 0, rd=0.120, ag=0, icc=7, m=-1'
   standa8(6) = '+  t, dm=0.070'

!! Definition of commercial Standa mount for large optics 8 inches.
   standa6(1) = 'h  t, 0.000001, 0, 0.124, 0, m=-1'
   standa6(2) = '+  t, dm=0.030'
   standa6(3) = 'd  d, 0.030, 0.0, rd=0.124, ag=0, icc=1, m=-1'
   standa6(4) = '+  d, dm=0.090'
   standa6(5) = 'c  t, 0.120, 0, rd=0.120, ag=0, icc=7, m=-1'
   standa6(6) = '+  t, rd=0.070/2, da=-90'
   standa6(7) = '+  t, rd=0.120, da=-180'
   standa6(8) = '+  t, rd=0.070/2, da=-270'
!   standa6(5) = 'd  t, 0.120, 0, rd=0.120, ag=0, icc=7, m=-1'
!   standa6(6) = '+  t, dm=0.070'


!! Definition of galvo made by NIKHEF
      galvo(01) = 'c  r, 0., 0., rd=0.00635, ag=0., icc=14' !mirror (front)
      galvo(02) = ' + d, rd=(0.0128-0.0071)/2.,    da= -90.'
      galvo(03) = ' + d, rd=(0.02052-0.00635)/2.,  da=  -0.00001'
      galvo(04) = ' + d, rd=0.0128/2.,             da=-270.'
      galvo(05) = ' + d, rd=(0.039-0.02052)/2.,    da=-360.'
      galvo(06) = ' + d, rd=0.020/2.,              da=-450.'
      galvo(07) = ' + d, rd=0.0393,                da=-540.' !back
      galvo(08) = ' + d, rd=0.020/2.,              da=-630.'
      galvo(09) = ' + d, rd=(0.0393-0.02052)/2.,   da=-720.'
      galvo(10) = ' + d, rd=0.0128/2.,             da=-810.'
      galvo(11) = ' + d, rd=(0.02052-0.00635)/2.,  da=-720.'
      galvo(12) = ' + d, rd=(0.0128-0.0071)/2.,    da=-630.'

!! Definition of the minitowers for the SNEB, SWEB
   minitower1(01)='p side1=1.380,side2=1.384'
   minitower1(02)='c  d, -1.380/2, 0., 1.384/2, 0., m=-1, icc=-1, sc=8'  !door side1
   minitower1(03)=' + d, rd=(1.380-0.880)/2./2.,da=-90.,sc=8'    !start back side
   minitower1(04)=' + d, rd=0.073/2.,           da=-0.00001,sc=8'!  vp 880
   minitower1(05)=' + t, rd=0.880/2.,           da= -90.'        !  vp 880
   minitower1(06)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 880
   minitower1(07)=' + d, rd=(1.380-0.880)/2./2.,da= -90.,sc=8'
   minitower1(08)=' + d, rd=1.380/2.,           da=-180.,sc=8'   !door side2
   minitower1(09)=' + d, rd=(0.250-0.160/2)/2., da=-270.,sc=8'   !start front side
   minitower1(10)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 160
   minitower1(11)=' + t, rd=0.160/2.,           da=-270.'        !  vp 160
   minitower1(12)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 160
   minitower1(13)=' + d, rd=75e-3/2.,           da=-270.,sc=8'   !     (0.580-0.350/2.-0.250-0.160/2.)/2.
   minitower1(14)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 350
   minitower1(15)=' + t, rd=0.350/2.,           da=-270.'        !  vp 350
   minitower1(16)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 350
   minitower1(17)=' + d, rd=200e-3/2.,          da=-270.,sc=8'   !     (1.130-0.350/2-0.580-0.350/2.)/2.
   minitower1(18)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 350
   minitower1(19)=' + t, rd=0.350/2.,           da=-270.'        !  vp 350
   minitower1(20)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 350
   minitower1(21)=' + d, rd=75e-3/2.,           da=-270.,sc=8'   !     (1.380-1.130-0.350/2.)/2.

!! Definition of the minitowers for SPRB (same as SNEB,SWEB, except for the order of the front viewports)
   minitower2(01)='c  d, -1.380/2, 0., 1.384/2, 0., m=-1, icc=-1, sc=8'  !door side1
   minitower2(02)=' + d, rd=(1.380-0.880)/2./2.,da=-90.,sc=8'    !start back side
   minitower2(03)=' + d, rd=0.073/2.,           da=-0.00001,sc=8'!  vp 880
   minitower2(04)=' + t, rd=0.880/2.,           da= -90.'        !  vp 880
   minitower2(05)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 880
   minitower2(06)=' + d, rd=(1.380-0.880)/2./2.,da= -90.,sc=8'   
   minitower2(07)=' + d, rd=1.380/2.,           da=-180.,sc=8'   !door side2
   minitower2(08)=' + d, rd=75e-3/2.,           da=-270.,sc=8'   !start front side
   minitower2(09)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 350
   minitower2(10)=' + t, rd=0.350/2.,           da=-270.'        !  vp 350
   minitower2(11)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 350
   minitower2(12)=' + d, rd=200e-3/2.,          da=-270.,sc=8'   !     (1.130-0.350/2-0.580-0.350/2.)/2.
   minitower2(13)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 350
   minitower2(14)=' + t, rd=0.350/2.,           da=-270.'        !  vp 350
   minitower2(15)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 350
   minitower2(16)=' + d, rd=75e-3/2.,           da=-270.,sc=8'   !     (0.580-0.350/2.-0.250-0.160/2.)/2.
   minitower2(17)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 160
   minitower2(18)=' + t, rd=0.160/2.,           da=-270.'        !  vp 160
   minitower2(19)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 160
   minitower2(20)=' + d, rd=(0.250-0.160/2)/2., da=-270.,sc=8'   !

!! Definition of the minitowers for SDB2 and SIB2 (with two large viewports on each side)
   minitower3(01)='c  d, -1.380/2, 0., 1.384/2, 0., m=-1, icc=-1, sc=8'  !door side1
   minitower3(02)=' + d, rd=(1.380-0.880)/2./2.,da=-90.,sc=8'    !start back side
   minitower3(03)=' + d, rd=0.073/2.,           da=-0.00001,sc=8'!  vp 880
   minitower3(04)=' + t, rd=0.880/2.,           da= -90.'        !  vp 880
   minitower3(05)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 880
   minitower3(06)=' + d, rd=(1.380-0.880)/2./2.,da= -90.,sc=8'   
   minitower3(07)=' + d, rd=1.380/2.,           da=-180.,sc=8'   !door side2
   minitower3(08)=' + d, rd=(1.380-0.880)/2./2.,da=-270.,sc=8'   !start front side
   minitower3(09)=' + d, rd=0.073/2.,           da=-180.,sc=8'   !  vp 880
   minitower3(10)=' + t, rd=0.880/2.,           da= -270.'       !  vp 880
   minitower3(11)=' + d, rd=0.073/2.,           da=-360.,sc=8'   !  vp 880
   minitower3(12)=' + d, rd=(1.380-0.880)/2./2.,da=-270.,sc=8'   !  
  
  call oc_bind('PD', pd)
  call oc_bind('PD2', pd2)
  call oc_bind('CAM', cam)
  call oc_bind('CAM2', cam2)
  call oc_bind('QPD', qpd)
  call oc_bind('QPD2', qpd2)
  call oc_bind('GALVO', galvo)
  call oc_bind('ST8', standa8)
  call oc_bind('ST6', standa6)
  call oc_bind('M8822', m8822)
  call oc_bind('U200', u200)
  call oc_bind('U100', u100)
  call oc_bind('MINITOWER1', minitower1)
  call oc_bind('MINITOWER2', minitower2)
  call oc_bind('MINITOWER3', minitower3)


!##############################################################################
! First page:  Global Layout
100 continue
goto 200 !Skip this page, go to 2nd page

      call ps_msg(0,'  --------- STARTING GLOBAL LAYOUT SIMULATION AND DRAWING --------------  ')

      call oc_init(pform='A0_L',pid='',psout='AdV_Layout_ITF') ! initialize OPTOCAD A0 landscape 1189, 841
 
! Draw buildings
    call oc_frame(cfxs,cfys,cfxe,cfye,cfxof,cfyof,psc,cf,d_tl=30.,d_tm=8.,gld=1.,glc=lcg,glp=ltg) ! C house
    call oc_frame(wfxs,wfys,wfxe,wfye,wfxof,wfyof,psc,wf,gld=1.,glc=lcg,glp=ltg) ! W house
    call oc_frame(nfxs,nfys,nfxe,nfye,nfxof,nfyof,psc,nf,gld=1.,glc=lcg,glp=ltg) ! N house
    call oc_frame(mcfxs,mcfys,mcfxe,mcfye,mcfxof,mcfyof,psc,mcf,gld=1.,glc=lcg,glp=ltg) ! MC house

!  Draw tank platforms
do it=1,nt       ! do tanks
  if (it == 2) then
    i=mcf
  else if (it == 6) then
    i=nf
  else if (it == 8) then
    i=wf
  else
    i=cf
  end if
  call ps_quad(i,tank(1,it)-2,tank(2,it)-2,4.,4.,fill=0,ci=1)
end do

! Draw tubes, valves and cryotraps    
    call ps_quad(cf,tank(1,1),tank(2,1)-0.15,cfxs,.3,fill=0,ci=1)        ! MC tube (300mm beam tube diameter)
    call ps_quad(mcf,tank(1,2),tank(2,2)-0.15,100.,.3,fill=0,ci=1)       ! MC tube
    call ps_quad(cf,tank(1,1)-1.8,tank(2,1)-0.25,0.1,.5,ci=6,fill=0)     ! MC valve
    call ps_text(cf,'IMC valve',-3.2,-10.8,fs=5.,ci=6)
!    call ps_quad(cf,tank(1,1)-.13,tank(2,1),.26,tank(2,3)-tank(2,1))     ! IB-PR tube
!    call ps_quad(cf,tank(1,3)-.2,tank(2,3),.4,tank(2,4)-tank(2,3))       ! PR-BS tube
!    call ps_quad(cf,tank(1,4)-0.125,tank(2,4)-1.8,0.25,.1,ci=6)          ! PR valve
!    call ps_text(cf,'PRM valve',0.3,-1.8,fs=5.,ci=6)
!    call ps_quad(cf,tank(1,4)-.2,tank(2,4),.4,tank(2,5)-tank(2,4))       ! BS-NI tube
!    call ps_quad(cf,tank(1,5)-0.125,tank(2,5)-1.8,0.25,.1,ci=6)          ! NI-BS valve
!    call ps_text(cf,'IMX-BS valve',0.3,4.5,fs=5.,ci=6)
    call ps_quad(cf,tank(1,5)-.4,tank(2,5),.8,cfye-tank(2,5),fill=0,ci=1) ! NI-N tube
    call ps_quad(cf,-0.5,11.,1.,0.1,ci=6,fill=0)                          ! NI large valve
    call ps_text(cf,'NI large valve',0.7,11.,fs=5.,ci=6)
    call ps_quad(cf,-0.5,11.-2.7,1.,2.0,ci=6,fill=0)                      ! NI cryotrap
    call ps_text(cf,'NI cryotrap',0.7,10.,fs=5.,ci=6)
!    call ps_quad(cf,tank(1,4),tank(2,4)-.2,tank(1,7)-tank(1,4),.4)       ! BS-WI tube
!    call ps_quad(cf,tank(1,7)+1.8,tank(2,7)-0.125,0.1,.25,ci=6)          ! WI-BS valve
!    call ps_text(cf,'IMY-BS valve',-4.,-0.5,fs=5.,ci=6)
    call ps_quad(cf,tank(1,7),tank(2,7)-.4,cfxs-tank(1,7),.8,fill=0,ci=1) ! WI-W tube
    call ps_quad(cf,-11.,-0.5,0.1,1.,ci=6,fill=0)                         ! WI large valve
    call ps_text(cf,'WI large valve',-11.5,-0.8,fs=5.,ci=6)
    call ps_quad(cf,-11.+.7,-0.5,2.0,1.,ci=6,fill=0)                      ! WI cryotrap
    call ps_text(cf,'WI cryotrap',-10.,0.6,fs=5.,ci=6)
!    call ps_quad(cf,tank(1,4),tank(2,4)-.2,tank(1,9)-tank(1,4),.4)       ! BS-SR tube
!    call ps_quad(cf,tank(1,9),tank(2,10)-.2,tank(1,10)-tank(1,9),.4)     ! SR-DB tube
!    call ps_quad(cf,tank(1,4)+1.8,tank(2,4)-0.125,0.1,.25,ci=6)          ! SR valve
!    call ps_text(cf,'SRM valve',1.6,-0.5,fs=5.,ci=6)
    call ps_quad(wf,tank(1,8),tank(2,8)-.4,wfxe-tank(1,8),.8,fill=0,ci=1) ! WT-W tube
    call ps_quad(wf,tank(1,8)+6.0,tank(2,8)-0.5,0.1,1.,ci=6,fill=0)       ! WE large valve
    call ps_text(wf,'WE large valve',-3000.6,-0.8,fs=5.,ci=6)
    call ps_quad(wf,tank(1,8)+3.3,tank(2,8)-0.5,2.0,1.,ci=6,fill=0)       ! WE cryotrap
    call ps_text(wf,'WE cryotrap',-3002.,0.6,fs=5.,ci=6)
    call ps_quad(nf,tank(1,6)-.4,tank(2,6),.8,nfys-tank(2,6),fill=0,ci=1) ! NT-N tube
    call ps_quad(nf,tank(1,6)-0.5,tank(2,6)-6.0+0.756,1.,0.1,ci=6,fill=0) ! NE large valve
    call ps_text(nf,'NE large valve',0.7,3000.4,fs=5.,ci=6)
    call ps_quad(nf,tank(1,6)-0.5,tank(2,6)-6.0+1.456,1.,2.0,ci=6,fill=0) ! NE cryotrap
    call ps_text(nf,'NE cryotrap',0.7,3002.,fs=5.,ci=6)

!  Draw tanks:
do it=1,nt       ! do tanks
  if (it == 2) then
    i=mcf
  else if (it == 6) then
    i=nf
  else if (it == 8) then
    i=wf
  else
    i=cf
  end if
  call ps_quad(i,tank(1,it)-.5,tank(2,it)+.5,1.,1.,fill=0,ci=1)    ! N flange
  call ps_quad(i,tank(1,it)-.5,tank(2,it)-.5,1.,-1.,fill=0,ci=1)   ! S flange
  call ps_quad(i,tank(1,it)-.5,tank(2,it)-.5,-1.,1.,fill=0,ci=1)   ! W flange
  call ps_quad(i,tank(1,it)+.5,tank(2,it)-.5,1.,1.,fill=0,ci=1)    ! E flange 
  call ps_arc(i,tank(1,it),tank(2,it),trad-0.04,0.,twopi,fill=0,ci=0) !clean tank interior
  arcrange=pi/12.
  call ps_arc(i,tank(1,it),tank(2,it),trad,-arcrange,arcrange,rot=pi/4.,ci=1,lw=.6)
  call ps_arc(i,tank(1,it),tank(2,it),trad,-arcrange,arcrange,rot=3.*pi/4.,ci=1,lw=.6)
  call ps_arc(i,tank(1,it),tank(2,it),trad,-arcrange,arcrange,rot=5.*pi/4.,ci=1,lw=.6)
  call ps_arc(i,tank(1,it),tank(2,it),trad,-arcrange,arcrange,rot=7.*pi/4.,ci=1,lw=.6)
  call ps_plot(i,5,xy=cross,xu=tank(1,it),yu=tank(2,it),lw=.1)   ! centre
end do

! Draw benches
!    call ps_quad(cf,tank(1,15),tank(2,15),2.4,1.5,ci=lcben,fill=lcbni)     ! LB
    call ps_quad(cf,tank(1,15)+0.4,tank(2,15)+0.25,2.0,1.0,ci=lcben,fill=lcbni)     ! LB
    call ps_quad(cf,tank(1,11),tank(2,11),2.4,1.5,ci=lcben,fill=lcbni)    ! EIB
!    call ps_quad(cf,tank(1,11)+0.07,tank(2,11)+0.07,2.4-0.07,1.5-0.07,ci=lcben,fill=lcbni)    ! EIB_reduced_due_to_covers
    call ps_quad(cf,tank(1,27),tank(2,27),0.45,0.45,ci=lcben,fill=lcbni)    ! EAB bench
    call ps_quad(cf,tank(1,16),tank(2,16),0.85,0.1,ci=lcben,fill=lcbni)   ! EIB2
    call ps_plot(cf,13,x=xb+tank(1,1),y=yb+tank(2,1),ci=lcben,fill=lcbni) ! SIB1
    call ps_quad(cf,tank(1,1)+3.085-.65,tank(2,1)-.65,1.3,1.3,ci=lcben,fill=lcbni) ! SIB2
    call ps_quad(cf,tank(1,1)+3.085-.65+1.4,tank(2,1)-.65,0.3,1.3,ci=lcben,fill=lcbni)! SIB2 external
    call ps_quad(mcf,tank(1,19),tank(2,19),0.3,0.6,ci=lcben,fill=lcbni)          ! External mode cleaner bench
    call ps_plot(cf,npointsSDB1,x=xsdb1+tank(1,10),y=ysdb1+tank(2,10),ci=lcben,fill=lcbni)! SDB1
    call ps_quad(cf,tank(1,12)-1.3/2,tank(2,12)-1.3/2,1.3,1.3,ci=lcben,fill=lcbni)        ! SDB2
    call ps_quad(cf,tank(1,20)-2.0/2,tank(2,20)-0.9/2,2.0,0.9,ci=lcben,fill=lcbni)                    ! EDB2
    call ps_quad(wf,tank(1,13)-wbwidth/2,tank(2,13)-wbwidth/2,wbwidth,wbwidth,ci=lcben,fill=lcbni)   ! SWEB
    call ps_quad(wf,tank(1,21)-0.20/2,tank(2,21)-1.00/2,  0.20,1.00,ci=lcben,fill=lcbni)  ! EWEB1
    call ps_quad(wf,tank(1,22)-1.50/2,tank(2,22)-1.00/2,  1.50,1.00,ci=lcben,fill=lcbni)  ! EWEB2
    call ps_quad(nf,tank(1,14)-nbwidth/2,tank(2,14)-nbwidth/2,nbwidth,nbwidth,ci=lcben,fill=lcbni)   ! SNEB
    call ps_quad(nf,tank(1,23)-1.00/2,tank(2,23)-0.20/2,  1.00,0.20,ci=lcben,fill=lcbni)  ! ENEB1
    call ps_quad(nf,tank(1,24)-1.00/2,tank(2,24)-1.50/2,  1.00,1.50,ci=lcben,fill=lcbni)  ! ENEB2
    call ps_quad(cf,ref=ref0,xu=tank(1,25),yu=tank(2,25),wu=prbwidth,hu=prbwidth,ci=lcben,fill=lcbni,rot=prbrot) ! SPRB

    
!  Tank and table annotation:
    call ps_text(cf,'Laser Bench',tank(1,15), tank(2,15)-.25,fs=5.,ci=lcben)
    call ps_text(cf,'External Injection Bench',tank(1,11),tank(2,11)-.25,fs=5.,ci=lcben)
    call ps_text(cf,'EAB Bench',tank(1,27)+.5,tank(2,27)+.2,fs=5.,ci=lcben)
    call ps_text(cf,'Suspended Injection Bench 1',tank(1,1)+0.8,tank(2,1)-1.3,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(cf,'Suspended Injection Bench 2',tank(1,1)+4.3,tank(2,1)-1.3,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(mcf,'External Mode Cleaner Bench',tank(1,19)-0.3,tank(2,19)-1.0,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(cf,'Suspended Detection Bench 1',tank(1,10),tank(2,10)+1.21,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(cf,'Suspended Detection Bench 2',tank(1,12),tank(2,12)+1.0,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(cf,'External Detection Bench',   tank(1,20)+1.2,tank(2,20)+1.5,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(nf,'North End Bench (SNEB)',     tank(1,14)-1., tank(2,14)-1.5 ,fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(nf,'ENEB1',                      tank(1,23)-.25,tank(2,23)+0.15, fs=5.,ci=lcben)
    call ps_text(nf,'ENEB2',                      tank(1,24)+.8,tank(2,24)-0.5,   fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(wf,'West End Bench (SWEB)',      tank(1,13)-0.7, tank(2,13)-1.0, fs=5.,ci=lcben)
    call ps_text(wf,'EWEB1',                      tank(1,21)+0.1,tank(2,21)+0.7,  fs=5.,rot=pi/2.,ci=lcben)
    call ps_text(wf,'EWEB2',                      tank(1,22)-0.4,tank(2,22)+0.7,  fs=5.,ci=lcben)
    call ps_text(cf,'SPRB',                       tank(1,25)-0.5, tank(2,25)-1. ,fs=5.,rot=prbrot,ci=lcben)
    
    call oc_set(                     &
                dpr  = .001,         & ! default reflectivity for AR coatings
                dpt  = .999,         & ! default transmission for AR coatings
                rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                      1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                      1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                      1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                      1.507,         & ! n of BK7
                      1.94,          & ! TGG for Faraday isolators
                      2.1517,        & ! Lithium Niobate for Pockels cells
                      1.0,           & ! vacuum for diaphragms
                      1.53069/),     & ! Zerodur
                mcat = 3,            & ! assume cavities in resonance
                                !                mcat = 2,            & ! assume cavities in resonance
                ndcav= 6,            &
                                !                pctl = 1,            & ! print data in cavity test loop
                pind = 1,            & ! print interference data
                lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                nice = 5,           & ! Improve precision of eigenmode calculation
                fslb = 4.0,          & ! character height for annotation (mm)
                cslw = .05,           & ! linewidth for surface of components
                hslc = -1,           & ! line color of hidden surfaces
                nslc = 1,            & ! line color of neglected surfaces
                prsd = 1,            & ! switch off printing of ray-segment data
                pcad = 1,            & ! switch off printing of cavity data
                pclc = 0,            & ! switch off printing of lens curvatures
                wismin = 0.4) !,             & ! Allow for smaller than default (=2mm) waist indicators
!               write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &
!                print=printPar) !  'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb')

      call oc_input(ocd,nib)
    
      do ib=1,nib
         if (ib==beamMain) then !main beam
            call ps_msg(0,' --- Main eps file: beamMain, tracing ')
            
            call oc_trace(ib,save='# SIB1\_M13') ! trace all segments originating from...
                                ! ...beam #1 and save ray-segment data at '# SIB1\_M13'
            call oc_set(nslc=0) ! don't plot contours of 'n' surfaces
            do it=cf,mcf        ! plot everything in normal (no zoom) frames
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.0005,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.0005,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
               call oc_surf(lccs,0,lw=0.01,frm=it)  
            end do
            
            call oc_input((/'o -100., -100.'/),nib2,'# SIB1\_M13') ! RCB input beam
            call oc_trace(nib2,save='# SIB1\_M8, # SIB1\_M23') ! trace all segments originating from...    
                                ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
            call oc_set(nslc=0) ! don't plot contours of 'n' surfaces
            do it=cf,mcf        ! plot everything in normal (no zoom) frames
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
               call oc_surf(lccs,0,.1,frm=it)  
            end do
            
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M8') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #3
            call oc_set(nslc=0) ! don't plot contours of 'n' surfaces        
            do it=cf,mcf        ! plot everything in normal (no zoom) frames       
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
               call oc_surf(lccs,0,.1,frm=it)
            end do
            
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M23') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #4
            call oc_set(nslc=0) ! don't plot contours of 'n' surfaces        
            do it=cf,mcf        ! plot everything in normal (no zoom) frames       
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
               call oc_surf(lccs,0,.1,frm=it)
            end do
            
         else                   ! non-main beams
            call ps_msg(0,' --- Main eps file: not main beam --> skipping')
                                !Modify the wavelength for the auxiliary beams ??  TO BE CHECKED AND UPDATED !!!
            if (ib==beamHartmannDET) then
               call ps_msg(0,' --- Global layout eps file: beamHartmannDET  --> 800 nm')
               call oc_set(lambda=800e-9)
            else
               call ps_msg(0,' --- Global layout eps file: skipping beam!')
               cycle 
            end if
            call oc_trace(ib)   ! trace all segments originating from beam #1
                                ! Draw the beam in the BS, SPRB, PR-POP, SWEB, SNEB, NI-CP zoom frames 
            call oc_set(nslc=0) ! don't plot contours of 'n' surfaces
            do it=cf,mcf        ! plot everything in zoom frames
               call oc_beam(2.0,fill=lcbaux2,lc=lcbauxc2,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcbaux1,lc=lcbauxc1,lw=.1,frm=it)
               call oc_beam(0.0,lcbaux0,ltbaux0,.1,frm=it)
               call oc_surf(lccs,0,.1,frm=it)
            end do
                                !         call oc_reset            
         end if
      end do
      
                                ! Annotations
      call oc_frame(afxs,afys,afxe,afye,afxof,afyof,psc,af,fill=0,ax='') ! Annotations
      call ps_insert(af,'AdV_logo.eps',102.5,100.5,xsf=0.6,clip=0) ! Virgo Logo
      call ps_text(af,'Advanced VIRGO Layout, Otpocad Version \vt|'//trim(version)//  &
      '|\,, \date',xu=100.2,yu=103.5,fs=4.)
      call ps_text(af,'Romain Bonnand, Julien Marque, Loic Rolland, Mirko Prijatelj',xu=100.2,yu=103.2,fs=4.)
      call ps_text(af,'Global Layout',xu=100.2,yu=102.9,fs=4.)
      call oc_reset
      
      
!##############################################################################
! Second page:  Zoom Frames of Input Benches
200 continue
goto 300          ! Skip second page, go to third page

      call ps_msg(0,' --------- STARTING ZOOM ON INPUT BENCHES SIMULATION AND DRAWING -------------- ')

      call oc_init(pform='A0_L',pid='',psout='AdV_Layout_InputBenches') ! re-initialize OPTOCAD, two PS file

    call oc_set(                     &
                dpr  = .001,         & ! default reflectivity for AR coatings
                dpt  = .999,         & ! default transmission for AR coatings
                rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                      1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                      1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                      1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                      1.507,         & ! n of BK7
                      1.94,          & ! TGG for Faraday isolators
                      2.1517,        & ! Lithium Niobate for Pockels cells
                      1.0,           & ! vacuum for diaphragms
                      1.53069/),     & ! Zerodur
                mcat = 3,            & ! assume cavities in resonance
                ndcav= 6,            &
!                pctl = 1,            & ! print data in cavity test loop
                pind = 1,            & ! print interference data
                lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                nice =20,           & ! Improve precision of eigenmode calculation
                fslb = 20.,          & ! character height for annotation (mm)
                cslw = .1,           & ! linewidth for surface of components
                hslc = -1,           & ! line color of hidden surfaces
                nslc = 1,            & ! line color of neglected surfaces
                prsd = 1,            & ! switch off printing of ray-segment data
                pcad = 0,            & ! switch off printing of cavity data
                pclc = 0) !,            & ! switch off printing of lens curvatures
!               write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &
!                print=printPar)      !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb')

! Suspended Injection Bench 1 (Top) Zoom
    call oc_frame(ibzxs,ibzys,ibzxe,ibzye,ibzxof,ibzyof,pzc3,ibf,gld=.025,glc=lcg,glp=ltg)
    call ps_plot(ibf,13,x=xb+tank(1,1),y=yb+tank(2,1),ci=lcben)
    call ps_text(ibf,'Suspended Injection',-0.51,-11.41,fs=5.,ci=1)
    call ps_text(ibf,'Bench 1 (SIB1 top)',-0.51,-11.44,fs=5.,ci=1)

! Suspended Injection Bench 1 (Below) Zoom (Reference Cavity)
    call oc_frame(rczxs,rczys,rczxe,rczye,rczxof,rczyof,pzc3,rcf,gld=.025,glc=lcg,glp=ltg,ax='')
    call ps_axis(rcf,ax='Xx',beg=rczxs-rfci2,end=rczxe-rfci2,d_tm=5.)
    call ps_axis(rcf,ax='Yy',beg=rczys-rfci2,end=rczye-rfci2,d_tm=5.)
    call ps_plot(rcf,13,x=xb+tank(1,1)+rfci,y=yb+tank(2,1)+rfci,ci=lcben)
    call ps_insert(rcf,'RefCav.eps',rfci2-0.199-0.065,rfci2-10.785,xsf=.44,rot=-pi/2)
    call ps_text(rcf,'Suspended Injection',-0.51+rfci2,-11.41+rfci2,fs=5.,ci=1)
    call ps_text(rcf,'Bench 1 (SIB1 below)',-0.51+rfci2,-11.44+rfci2,fs=5.,ci=1)

! Suspended Injection Bench 2 Zoom
    call oc_frame(ib2zxs,ib2zys,ib2zxe,ib2zye,ib2zxof,ib2zyof,pzc3,ib2f,gld=.025,glc=lcg,glp=ltg)
    call ps_quad(ib2f,tank(1,26)-.65,tank(2,26)-.65,1.3,1.3,ci=lcben)
    call ps_text(ib2f,'Suspended Injection Bench 2 (SIB2)',2.45,-11.69,fs=5.,ci=1)
    call ps_quad(ib2f,tank(1,26)-.65+1.4,tank(2,26)-.65,0.3,1.3,ci=lcben)

! Laser Bench Zoom
!    call oc_frame(lbzxs,lbzys,lbzxe,lbzye,lbzxof,lbzyof,pzc3,lbf,gld=.05,glc=lcg,glp=ltg)
!    call ps_quad(lbf,tank(1,15),tank(2,15),2.,1.,ci=lcben)
!    call ps_text(lbf,'Laser Bench',-4.54,-13.91,fs=10.,ci=1)

! External Injection Bench 1 Zoom
    call oc_frame(eibzxs,eibzys,eibzxe,eibzye,eibzxof,eibzyof,pzc3,eibf,gld=.025,glc=lcg,glp=ltg)
    call ps_quad(eibf,tank(1,11),tank(2,11),2.4,1.5,ci=lcben)
!    call ps_quad(eibf,tank(1,11)+0.07,tank(2,11)+0.07,2.4-0.14,1.5-0.14,ci=lcben,fill=lcbni)    ! EIB_reduced_due_to_covers
    call ps_quad(eibf,tank(1,11)+0.79,tank(2,11)+1.5-0.06,0.04,0.04,ci=1)                       ! Pillar
    call ps_quad(eibf,tank(1,11)+2.4-0.86-0.2,tank(2,11)+1.5-0.06,0.04,0.04,ci=1)                       ! Pillar
    call ps_text(eibf,'External Injection Bench 1 (EIB1)',-1.79,-13.80,fs=10.,ci=1)
    call ps_quad(eibf,tank(1,11)+.75,tank(2,11)+.02,0.6,0.6,ci=1)                           ! IBMS
    call ps_text(eibf,'Input Beam Monitoring System',tank(1,11)+.9,tank(2,11)+.60,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+1.38,tank(2,11)+.02,1.0,0.4,ci=1)                          ! Hartmann
    call ps_text(eibf,'TCS Hartmann setup',tank(1,11)+1.48,tank(2,11)+.15,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+0.75,tank(2,11)+.621,0.3,0.22,ci=1)                        ! PhaseCamera
    call ps_text(eibf,'Phase camera',tank(1,11)+.8,tank(2,11)+.67,fs=4.,ci=1)
    
! EAB Bench 1 Zoom
    call oc_frame(eabzxs,eabzys,eabzxe,eabzye,eabzxof,eabzyof,pzc3,tempf,gld=.025,glc=lcg,glp=ltg)
    call ps_quad(tempf,tank(1,27),tank(2,27),0.45,0.45,ci=lcben)
    call ps_text(tempf,'External Aux. Bench 1 (EAB1)',tank(1,27)+0.01,tank(2,27)+eabzyrange-0.07,fs=5.,ci=1)
    
! External Injection Bench 2 Zoom
    call oc_frame(eib2zxs,eib2zys,eib2zxe,eib2zye,eib2zxof,eib2zyof,pzc3,eib2f,gld=.025,glc=lcg,glp=ltg)
    call ps_quad(eib2f,tank(1,16),tank(2,16),0.85,0.1,ci=lcben)
    call ps_text(eib2f,'External Injection Bench 2 (EIB2)',tank(1,16),tank(2,16)-.03,fs=5.,ci=1)

! External Injection Bench 3 Zoom

! External Reference Cavity Bench Zoom
    call oc_frame(erczxs,erczys,erczxe,erczye,erczxof,erczyof,pzc3,ercf,gld=.025,glc=lcg,glp=ltg,ax='')
    call ps_axis(ercf,ax='Xx',beg=erczxs-rfci2,end=erczxe-rfci2,d_tm=5.)
    call ps_axis(ercf,ax='Yy',beg=erczys-rfci2,end=erczye-rfci2,d_tm=5.)
    call ps_quad(ercf,tank(1,18),tank(2,18),1.2,0.45,ci=lcben)
    call ps_text(ercf,'External Reference Cavity Bench',-0.99+rfci2,-13.15+rfci2,fs=5.,ci=1)

! Mode Cleaner end mirror Zoom
    call oc_frame(mczxs,mczys,mczxe,mczye,mczxof,mczyof,pzc3,mcemf,gld=.025,glc=lcg,glp=ltg)
    call ps_text(mcemf,'Input Mode Cleaner End Mirror',tank(1,2)-0.19,-11.29,fs=5.,ci=1)

! External Mode Cleaner Bench Zoom
    call oc_frame(emcbzxs,emcbzys,emcbzxe,emcbzye,emcbzxof,emcbzyof,pzc3,emcbf,gld=.025,glc=lcg,glp=ltg)
    call ps_quad(emcbf,tank(1,19),tank(2,19),0.3,0.6,ci=lcben)
    call ps_text(emcbf,'External Mode Cleaner Bench',tank(1,19),-11.415,fs=4.,ci=1)


    call oc_input(ocd,nib)

    do ib=1,nib
       if (ib==beamMain) then   !Main beam
          call ps_msg(0,' --- INJ benches eps file: mainBeam --> tracing') 
          call oc_trace(ib,save='# SIB1\_M13') ! trace all segments originating from...
                                ! ...beam #1 and save ray-segment data at '# SIB1\_M13'

          ! plot all zoom frames
          do it=ibf,emcbf
             call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
             call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
             call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
             call oc_surf(lccs,0,.1,frm=it) 
          end do

          call oc_input((/'o -100., -100.'/),nib2,'# SIB1\_M13') ! RCB input beam
          call oc_trace(nib2,save='# SIB1\_M8,# SIB1\_M23') ! trace all segments originating from...
                                          ! ...beam #2 and save ray-segment data at '# SIB1\_M8,# SIB1\_M23'

          ! plot reference cavity zoom frames
          do it=rcf,ercf         
             call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
             call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
             call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
             call oc_surf(lccs,0,.1,frm=it)    
          end do

          call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M8') ! SIB1 input beam
          call oc_trace(nib2)    ! trace all segments originating from beam #3

          ! plot all zoom frames
          do it=ibf,emcbf
             call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
             call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
             call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
             call oc_surf(lccs,0,.1,frm=it) 
          end do
          
          call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M23') ! SIB1 input beam
          call oc_trace(nib2)    ! trace all segments originating from beam #4

          ! plot all zoom frames
          do it=ibf,emcbf
             call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
             call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
             call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
             call oc_surf(lccs,0,.1,frm=it) 
          end do
       else                     ! Other beams
          call ps_msg(0,' --- INJ benches eps file: not main beam  --> not drawn')
          cycle
       end if
    end do

! Draw IPC1, IPC2, IPC3, Faraday, IBMS, Hartmann and Pstab boxes
!    call ps_quad(eibf,tank(1,11)+0.213,tank(2,11)+0.66,0.08,-0.21,ci=1,fill=12)         ! IPC1
!    call ps_text(eibf,'IPC1',tank(1,11)+.229,tank(2,11)+0.55,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+0.095,tank(2,11)+1.28,0.08,-0.15,ci=1,fill=12)         ! IPC1
    call ps_text(eibf,'IPC1',tank(1,11)+.11,tank(2,11)+1.15,fs=4.,ci=1)
!    call ps_quad(eibf,tank(1,11)+0.44,tank(2,11)+0.23,-0.21,0.08,ci=1,fill=12)         ! IPC1
!    call ps_text(eibf,'IPC1',tank(1,11)+0.31,tank(2,11)+0.26,fs=4.,ci=1)
    
    
    call ps_quad(ibf,tank(1,1)-.04+.225,tank(2,1)+.200,0.08,-0.21,ci=1,fill=12)      ! IPC2
    call ps_text(ibf,'IPC2',tank(1,1)-.045+.225+.02,tank(2,1)+.080,fs=4.,ci=1)
    call ps_quad(ib2f,tank(1,1)+3.085-.4,tank(2,1)-.6-.04+25e-3,.21,.08,ci=1,fill=12)      ! IPC3
    call ps_text(ib2f,'IPC3',tank(1,1)+3.085-.4+.05,tank(2,1)-.6-.01+25e-3,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+1.2,tank(2,11)+1.215,0.1,0.05,ci=1,fill=12)          ! IPC4
    call ps_text(eibf,'IPC4',tank(1,11)+1.22,tank(2,11)+1.23,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+1.15,tank(2,11)+1.115,0.1,0.05,ci=1,fill=12)        ! IPC5
    call ps_text(eibf,'IPC5',tank(1,11)+1.17,tank(2,11)+1.13,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+1.27,tank(2,11)+.98,0.1,0.05,ci=1,fill=12)            ! IPC6
    call ps_text(eibf,'IPC6',tank(1,11)+1.29,tank(2,11)+.995,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+0.585,tank(2,11)+.61,0.05,0.1,ci=1,fill=12)           ! IPC7
    call ps_text(eibf,'IPC7',tank(1,11)+0.586,tank(2,11)+.635,fs=4.,ci=1)
    call ps_quad(eibf,tank(1,11)+1.25,tank(2,11)+.675,0.1,0.05,ci=1,fill=12)          ! IPC8
    call ps_text(eibf,'IPC8',tank(1,11)+1.27,tank(2,11)+0.69,fs=4.,ci=1)
    call ps_quad(ibf,tank(1,1)-.04+.35,tank(2,1)-.24,0.08,0.48,ci=1,fill=12)         ! SIB Faraday
    call ps_quad(ibf,tank(1,1)-.09+.35,tank(2,1)-.155,0.18,0.18,ci=1,fill=12)     
    call ps_text(ibf,'SIB\_F',tank(1,1)+.32,tank(2,1)-.18+.1,fs=4.,ci=1)
    call ps_quad(rcf,tank(1,1)-100.+.145,tank(2,1)-100.-.16,0.25,0.32,ci=1,fill=12)  ! Pstab
    call ps_text(rcf,'Power Stabilization Box',tank(1,1)-100.+.155,tank(2,1)-100.-.01,fs=4.,ci=1)


! Draw mirrors of periscopes
    call ps_arc(ibf,tank(1,1)+.35,tank(2,1)+.37,rux=.025,ruy=.025/1.414,ci=1,fill=icc)          ! SIB1_M7
    call ps_arc(rcf,tank(1,1)-100+.35,tank(2,1)-100+.37,rux=.025/1.414,ruy=.025,ci=1,fill=icc)  ! SIB1_M8
    call ps_arc(rcf,tank(1,1)-100-.245,tank(2,1)-100+.435,rux=.025,ruy=.025/1.414,ci=1,fill=icc)! SIB1_M12
    call ps_arc(ibf,tank(1,1)-.245,tank(2,1)+.435,rux=.025,ruy=.025/1.414,ci=1,fill=icc)        ! SIB1_M13
    call ps_arc(rcf,tank(1,1)-100+.285,tank(2,1)-100.225,rux=.0125,ruy=.0125/1.414,ci=1,fill=icc)! SIB1_M18a
    call ps_arc(rcf,tank(1,1)-100+.285,tank(2,1)-100.225,rux=.0125/1.414,ruy=.0125,ci=1,fill=icc)! SIB1_M18b
    call ps_arc(rcf,tank(1,1)-100+.30,tank(2,1)-100-.40,rux=.025/1.414,ruy=.025,ci=1,fill=icc)  ! SIB1_M21
    call ps_arc(ibf,tank(1,1)+.30,tank(2,1)-.40,rux=.025/1.414,ruy=.025,ci=1,fill=icc)          ! SIB1_M22

! Annotations
    call oc_frame(afxs,afys,afxe,afye,afxof,afyof,psc,af,fill=0,ax='')    ! Annotations
    call ps_insert(af,'AdV_logo.eps',102.5,100.5,xsf=0.6,clip=0)          ! Virgo Logo
    call ps_text(af,'Advanced VIRGO Layout, Optocad Version \vt|'//trim(version)//  &
                    '|\,, \date',xu=100.2,yu=103.5,fs=4.)
    call ps_text(af,'Julien Marque, Mirko Prijatelj',xu=100.2,yu=103.2,fs=4.)
    call ps_text(af,'Layout of Input Benches',xu=100.2,yu=102.9,fs=4.)
    call oc_reset

!!!!!!!!!!! add annotations about colors: blue is photodiode, green is quadrant, ...
!##############################################################################
! New Layout (3):  Zooom on BS, PR and End Benches
!##############################################################################

300 continue
goto 400          !skip this page
  
      call ps_msg(0,' --------- STARTING ZOOM ON BS, PR, END MIRROR and END BENCHES SIMULATION AND DRAWING -------------- ')

      call oc_init(pform='A0_L',pid='',psout='AdV_Layout_EndBenches') ! re-initialize OPTOCAD, two PS file
                                !      call oc_reset
      !  pst=(/0.1,.01,.001,.0001,.00001,0.000001/)    ! Power thresholds for color steps
      !  pst=(/1e-10,1e-11,1e-12,1e-13,1e-14,1e-15/)    ! Power thresholds for color steps
    
! Beamsplitter Zoom new geometry
    call oc_frame(bszxs,bszys,bszxe,bszye,bszxof,bszyof,pez,bsf,d_tm=4.,gld=.1,glc=lcg,glp=ltg) ! BS frame 
    call ps_text(bsf,'BS Mirror',bszxs+0.2*(bszxe-bszxs),bszye-0.10,fs=8.,ci=1)                                      ! BS frame legend
    ! call ps_quad(bsf,0.19,0.35,0.135,.03,fill=0)                                                                     ! ???

! North Bench Zoom
    call oc_frame(nbzxs,nbzys,nbzxe,nbzye,nbzxof,nbzyof,pez,nbf,gld=.05,glc=lcg,glp=ltg)                             ! NE frame
    call ps_text(nbf,'X End Benches (SNEB, ENEB1, ENEB2)',nbzxs+0.1,nbzye-0.10,fs=8.,ci=1)                           ! NE frame legend
    call ps_quad(nbf,tank(1,14)-nbwidth/2,tank(2,14)-nbwidth/2,nbwidth,nbwidth,ci=lcben)                             ! SNEB bench shape
    call ps_quad(nbf,tank(1,23)-1.00/2,tank(2,23)-0.20/2,  1.00,0.20,ci=lcben)                                       ! ENEB1 bench shape
    call ps_quad(nbf,tank(1,24)-1.00/2,tank(2,24)-1.50/2,  1.00,1.50,ci=lcben)                                       ! ENEB2 bench shape
    
! West Bench Zoom
    call oc_frame(wbzxs,wbzys,wbzxe,wbzye,wbzxof,wbzyof,pez,wbf,gld=.05,glc=lcg,glp=ltg)                             ! WE frame
    call ps_text(wbf,'Y End Benches (SWEB, EWEB1, EWEB2)',wbzxs+2.,wbzye-0.10,fs=8.,ci=1)                            ! WE frame legend
    call ps_quad(wbf,tank(1,13)-wbwidth/2,tank(2,13)-wbwidth/2,wbwidth,wbwidth,ci=lcben)                             ! SWEB bench shape
    call ps_quad(wbf,tank(1,21)-0.20/2,tank(2,21)-1.00/2,  0.20,1.00,ci=lcben)                                       ! EWEB1 bench shape
    call ps_quad(wbf,tank(1,22)-1.50/2,tank(2,22)-1.00/2,  1.50,1.00,ci=lcben)                                       ! EWEB2 bench shape

! PR Bench Zoom
    call oc_frame(prbzxs,prbzys,prbzxe,prbzye,prbzxof,prbzyof,pez,prbf,gld=.05,glc=lcg,glp=ltg)                      ! PR bench frame
    call ps_text(prbf,'PR Benches (SPRB, EPRB1)',prbzxs+0.3,prbzye-0.10,fs=8.,ci=1)                                  ! PR bench frame legend
    call ps_quad(prbf,ref=ref0,xu=tank(1,25),yu=tank(2,25),wu=prbwidth,hu=prbwidth,ci=lcben,rot=prbrot)              ! SPRB bench shape    
    call ps_quad(prbf,ref=ref0,xu=tank(1,25)-1.8*sin(prbrot)/2.,yu=tank(2,25)+1.8*cos(prbrot)/2., &
      wu=1.0,hu=0.20,ci=lcben,rot=prbrot)                                                                           ! EPRB1 bench shape

! PR and PRPOP Zoom
      call oc_frame(prmzxs, prmzys, prmzxe, prmzye, prmzxof, prmzyof,pez,prmf,gld=.05,glc=lcg,glp=ltg)
      call ps_text(prmf,'PR Mirror and POP',prmzxs+0.1,prmzye+0.05,fs=8.,ci=1)
      
! NE Zoom 
      call oc_frame(nemzxs, nemzys, nemzxe, nemzye, nemzxof, nemzyof,pez,nemf,gld=.05,glc=lcg,glp=ltg)
      call ps_text(nemf,'NE Mirror',nemzxs+0.1,nemzye+0.05,fs=8.,ci=1)

! WE Zoom 
      call oc_frame(wemzxs, wemzys, wemzxe, wemzye, wemzxof, wemzyof,pez,wemf,gld=.05,glc=lcg,glp=ltg)
      call ps_text(wemf,'WE Mirror',wemzxs+0.1,wemzye+0.05,fs=8.,ci=1)

! NI and NI CP Zoom 
      call oc_frame(nimzxs, nimzys, nimzxe, nimzye, nimzxof, nimzyof,pez,nimf,gld=.05,glc=lcg,glp=ltg)
      call ps_text(nimf,'NI Mirror and CP',nimzxs+0.1,nimzye+0.05,fs=8.,ci=1)
      
! WI and NI CP Zoom 
      call oc_frame(wimzxs, wimzys, wimzxe, wimzye, wimzxof, wimzyof,pez,wimf,gld=.05,glc=lcg,glp=ltg)
      call ps_text(wimf,'WI Mirror and CP',wimzxs+0.1,wimzye+0.05,fs=8.,ci=1)
      

      call oc_set(                   &
                dpr  = .001,         & ! default reflectivity for AR coatings
                dpt  = .999,         & ! default transmission for AR coatings
                rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                      1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                      1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                      1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                      1.507,         & ! n of BK7
                      1.94,          & ! TGG for Faraday isolators
                      2.1517,        & ! Lithium Niobate for Pockels cells
                      1.0,           & ! vacuum for diaphragms
                      1.53069/),     & ! Zerodur
                mcat = 3,            & ! assume cavities in resonance
                ndcav= 6,            &
                ! pctl = 1,           & ! print data in cavity test loop
                pind = 1,            & ! print interference data
                lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                nice =20,           & ! Improve precision of eigenmode calculation
                fslb = 3.0,          & ! character height for annotation (mm)  !!SHOULD BE 20 AND ADAPT THE SIZE IN OCD !!
                cslw = .1,           & ! linewidth for surface of components
                hslc = -1,           & ! line color of hidden surfaces
                nslc = 1,            & ! line color of neglected surfaces
                prsd = 1,            & ! switch off printing of ray-segment data
                pcad = 1,            & ! switch off printing of cavity data
                pclc = 0) !,            & ! switch off printing of lens curvatures
!                write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &
!                print=printPar) 


! Simulate the beams
      call oc_input(ocd,nib)
    
      do ib=1,nib
         if (ib==beamMain) then
            call ps_msg(0,' --- End benches eps file: beamMain --> tracing')
            !         call oc_set(lambda=1.064e-6)
            call oc_trace(ib,save='# SIB1\_M13') ! trace all segments originating from...
                                ! ...beam #1 and save ray-segment data at '# SIB1\_M13'
            call oc_input((/'o -100., -100.'/),nib2,'# SIB1\_M13') ! RCB input beam
            call oc_trace(nib2,save='# SIB1\_M8,# SIB1\_M23') ! trace all segments originating from...
                                ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M23') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #4
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M8') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #3
            
            
            ! Draw the beam in the BS, SPRB, PR-POP, SWEB, SNEB, NI-CP zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces
            do it=bsf,wimf      ! plot everything in zoom frames
               call oc_beam(3.0,fill=lcb3,lc=lcbc3,lw=.1,frm=it)
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,lw=.1,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,frm=it) !Plot beam axes in black, and no ray-segment number
               !call oc_beam(0.0,lcb0,ltb0,.1,rns=2.5,rnc=2,frm=it) !Plot beam axes in black, and ray-segment numbers in red
               call oc_surf(lccs,0,.1,frm=it) ! plot all surfaces
            end do
                                !  call oc_reset
            
         else                   !Other beams
            call ps_msg(0,' --- End benches eps file: ib>1, tracing also non-main beams')
            
                                !Modify the wavelength for the auxiliary beams ??  TO BE CHECKED AND UPDATED !!!
            if (ib==beamHartmannDET) then
               call ps_msg(0,' --- End benches eps file: beamHartmannDET --> 800 nm')
               call oc_set(lambda=800e-9)
            elseif (ib==beamAuxSNEB) then
               call ps_msg(0,' --- End benches eps file: beamAuxSNEB --> 630 nm ??')
               call oc_set(lambda=630e-9)
            end if
            
            call oc_trace(ib)   ! trace all segments originating from beam #1
                                ! Draw the beam in the BS, SPRB, PR-POP, SWEB, SNEB, NI-CP zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces
            do it=bsf,wimf      ! plot everything in zoom frames
               call oc_beam(2.0,fill=lcbaux2,lc=lcbauxc2,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcbaux1,lc=lcbauxc1,lw=.1,frm=it)
               call oc_beam(0.0,lcbaux0,ltbaux0,.1,frm=it)
               call oc_surf(lccs,0,.1,frm=it)
            end do
            !         call oc_reset            
         end if         
      end do
      
      ! Annotations
      call oc_frame(afxs,afys,afxe,afye,afxof+50,afyof,psc,af,fill=0,ax='') ! Annotations
                                !call ps_quad(af,99.,99.,13.,10.,fill=0)                              ! clean area
      call ps_insert(af,'AdV_logo.eps',102.5,100.5,xsf=0.6,clip=0) ! Virgo Logo
      call ps_text(af,'Advanced VIRGO Layout, Optocad Version \vt|'//trim(version)//  &
      '|\,, \date',xu=100.2,yu=103.5,fs=4.)
      call ps_text(af,'Romain Bonnand, Loic Rolland',xu=100.2,yu=103.2,fs=4.)
      call ps_text(af,'Layout of Output Benches',xu=100.2,yu=102.9,fs=4.)
      call oc_reset
      
!##############################################################################
! New Layout (4):  Zoom on dark fringe benches 
!##############################################################################

400 continue
goto 500   !skip this page
    
      call ps_msg(0,' --------- STARTING DARK FRINGE BENCHES SIMULATION AND DRAWING -------------- ')
      
      call oc_init(pform='A0_L',pid='',psout='AdV_Layout_SDBs') ! re-initialize OPTOCAD
                                !      call oc_reset
      !    pst=(/0.1,.01,.001,.0001,.00001,0.000001/)    ! Power thresholds for color steps
      
! Detection Bench Zoom (SDB1)
      call oc_frame(dbxs,dbys,dbxe,dbye,dbxof,dbyof-45,0.30,dbf,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(dbf,'Suspended Detection Bench (SDB1)',dbxs,dbys-0.10,fs=8.,ci=1) !title
      call ps_plot(dbf,npointsSDB1,x=xsdb1+tank(1,10),y=ysdb1+tank(2,10),ci=lcben) !bench shape 
      ! Positions of suspension fixations from Pierre Mugnier (30/04/2012)
      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971-0.0000-0.032,-0.002-0.300-0.020,xsf=0.30,rot=0.,clip=1) !suspension fixation point
      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971-0.2500-0.032,-0.002+0.150-0.020,xsf=0.30,rot=0.,clip=1) !suspension fixation point
      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971+0.2500+0.020,-0.002+0.150-0.032,xsf=0.30,rot=pi/2.,clip=1) !suspension fixation point
      call ps_insert(dbf,'SDB1_FaradayIsolator_scale0d5.eps',10.81,0.260,xsf=2*0.30,rot=0.,clip=1) !Faraday isolator and mounts
      call ps_insert(dbf,'SDB1_Telescope_2012_11.eps',10.971-0.880/2-0.025,-0.002+0.018-0.2823,xsf=0.30,rot=0.,clip=1) !Mounts of MMT
      !Positions of legs and rotation (x0,y0,a): (-0.44,0.182;-90-22.5)  (0.182,0.44;180-22.5)   (0.44,-0.182;90-22.5)   (-0.182,-0.44;-22.5)
      ! 0 is defined as the point of the leg that is in contact with the bench angle.
      !Position of figure: x = x0-Lcos(a)+lsin(a)   y=y0-Lsin(a)-lcos(a)   with L = 0.0624 m and l=0.005 m  (offset of the figure corner from 0)
      call ps_insert(dbf,'SDB1_Leg.eps',10.550, 0.240,xsf=0.30,rot=-1.963,clip=1) !Leg to fix the bench to the marionette
      call ps_insert(dbf,'SDB1_Leg.eps',11.213, 0.419,xsf=0.30,rot=+2.749,clip=1) !Leg to fix the bench to the marionette
      call ps_insert(dbf,'SDB1_Leg.eps',11.392,-0.244,xsf=0.30,rot=+1.178,clip=1) !Leg to fix the bench to the marionette
      call ps_insert(dbf,'SDB1_Leg.eps',10.729,-0.423,xsf=0.30,rot=-0.393,clip=1) !Leg to fix the bench to the marionette
      call ps_insert(dbf,'SDB1_TranslationStageLens.eps',10.971+0.240,-0.002+0.309,xsf=0.30,rot=pi,clip=1) !L2 translation stage
      call ps_insert(dbf,'SDB1_TranslationStageLens.eps',10.971+0.125,-0.002+0.218,xsf=0.30,rot=-pi/2,clip=1) !L3 translation stage


! Detection Bench Zoom (SDB2)
      call oc_frame(db2zxs-1.0,db2zys,db2zxe+0.10,db2zye,db2zxof-175,db2zyof-25,0.30,db2f,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(db2f,'Suspended Detection Bench (SDB2)',db2zxs,db2zye+0.05,fs=8.,ci=1) !title
      call ps_quad(db2f,tank(1,12)-1.30/2,tank(2,12)-1.30/2,1.30,1.30,ci=lcben) !bench shape

!! Link Between SDB1 and SDB2 benches Zoom
!      call oc_frame(dbxe,dbys,db2zxs,dbye,dlinkxof+750,dlinkyof-75,0.20,dlinkf,gld=.025,glc=lcg,glp=ltg) !frame
!      call ps_text(dlinkf,'Detection Link (SDB1->SDB2)',dbxe,dbye+0.05,fs=8.,ci=1) !title

! External Detection Bench (EDB)
      call oc_frame(ebzxs,ebzys,ebzxe,ebzye,ebzxof-575,ebzyof+100,0.30,ebf,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(ebf,'External Detection Bench (EDB)',ebzxs,ebzye+0.05,fs=8.,ci=1) !title
      call ps_quad(ebf,tank(1,20)-2.0/2,tank(2,20)-0.90/2,2.0,0.90,ci=lcben) !bench shape
      
      call oc_set(                     &
                  dpr  = .001,         &    ! default reflectivity for AR coatings
                  dpt  = .999,         & ! default transmission for AR coatings
                  rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                        1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                        1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                        1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                        1.507,         & ! n of BK7
                        1.94,          & ! TGG for Faraday isolators
                        2.1517,        & ! Lithium Niobate for Pockels cells
                        1.0,           & ! vacuum for diaphragms
                        1.53069/),     & ! Zerodur
                  mcat = 3,            & ! assume cavities in resonance
                                  !                mcat = 2,            & ! assume cavities in resonance
                  ndcav= 6,            &
                                  !                pctl = 1,            & ! print data in cavity test loop
                  pind = 1,            & ! print interference data
                  lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                  nice = 20,           & ! Improve precision of eigenmode calculation
                  fslb = 15.0,          & ! character height for annotation (mm)
                  cslw = .1,           & ! linewidth for surface of components
                  hslc = -1,           & ! line color of hidden surfaces
                  nslc = 1,            & ! line color of neglected surfaces
                  prsd = 1,            & ! switch off printing of ray-segment data
                  pcad = 1,            & ! switch off printing of cavity data
                  pclc = 0) !,            & ! switch off printing of lens curvatures
!                  write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                 write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &
!                  print=printPar) !  'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb')
                 
! Simulate the beams
      call oc_input(ocd,nib)
      
      do ib=1,nib
         if (ib==beamMain) then 
            call ps_msg(0,' --- SDB benches eps file: beamMain --> tracing')
            !         call oc_set(lambda=1.064e-6)
            call oc_trace(ib,save='# SIB1\_M13') ! trace all segments originating from...
                                ! ...beam #1 and save ray-segment data at '# SIB1\_M13'
            call oc_input((/'o -100., -100.'/),nib2,'# SIB1\_M13') ! RCB input beam
            call oc_trace(nib2,save='# SIB1\_M8,# SIB1\_M23') ! trace all segments originating from...
                                ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M23') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #4      
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M8') ! SIB1 input beam
            call oc_trace(nib2)
            
            
            ! Draw the beam in the End Benches zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces    
            do it=dbf,ebf    ! plot everything in zoom frames=
               call oc_beam(3.0,fill=lcb3,lc=lcbc3,lw=.1,frm=it) !Outer part, ...
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,lw=.1,frm=it) !Middle part, ...
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,lw=.1,frm=it) !Inner part of beam
               !call oc_beam(0.0,lcb0,ltb0,.1,frm=it) !Plot beam axes in black, and no ray-segment number
               call oc_beam(0.0,lcb0,ltb0,.1,rns=2.5,rnc=2,frm=it) !Plot beam axes in black, and ray-segment numbers in red
               call oc_surf(lccs,0,.1,frm=it) ! plot all surfaces
            end do
                                ! call oc_reset
                                ! call oc_reset
            
         else                   !Other beams
            call ps_msg(0,' --- SDB benches eps file: tracing also some non-main beams')
                                !Does it properly modify the wavelength for the auxiliary beams ??  TO BE CHECKED !!!
            if (ib==beamHartmannDET) then
               call ps_msg(0,' --- SDB benches eps file: beamHartmannDET  --> 800 nm (TBC)')
               call oc_set(lambda=790e-9)
            else                !if (ib==beamAuxSNEB) then
               call ps_msg(0,' --- SDB benches eps file: skip this beam !')
               cycle
            end if
            call oc_trace(ib)   ! trace all segments originating from beam #1
                                ! Draw the beam in the BS, SPRB, PR-POP, SWEB, SNEB, NI-CP zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces
            do it=dbf,ebf    ! plot everything in zoom frames
               !            call oc_beam(3.0,fill=lcbaux3,lc=lcbauxc3,lw=.1,frm=it)
               call oc_beam(2.0,fill=lcbaux2,lc=lcbauxc2,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcbaux1,lc=lcbauxc1,lw=.1,frm=it)
               call oc_beam(0.0,lcbaux0,ltbaux0,.1,rns=2.5,rnc=4,frm=it) !Plot beam axes in black, and no ray-segment number
                                !call oc_beam(0.0,lcbaux0,ltbaux0,.1,rns=2.5,rnc=3,frm=it) !Plot beam axis in black, and ray-segments numbers in green
               call oc_surf(lccs,0,.1,frm=it)
            end do
                                !call oc_reset
            
         end if
         
      end do
      
      ! Annotations
      call oc_frame(afxs,afys,afxe,afye,afxof,afyof,psc,af,fill=0,ax='') ! Annotations
                                !call ps_quad(af,99.,99.,13.,10.,fill=0)                              ! clean area
      call ps_insert(af,'AdV_logo.eps',102.5,100.5,xsf=0.6,clip=0) ! Virgo Logo
      call ps_text(af,'Advanced VIRGO Layout, Optocad Version \vt|'//trim(version)//  &
      '|\,, \date',xu=100.2,yu=103.5,fs=4.)
      call ps_text(af,'Romain Bonnand, Loic Rolland',xu=100.2,yu=103.2,fs=4.)
      call ps_text(af,'Layout of Output Benches',xu=100.2,yu=102.9,fs=4.)
      call oc_reset
      


!##############################################################################
! New Layout (5):  Zoom on OMC
!##############################################################################

500  continue
goto 600                  !skip this page
      
      call ps_msg(0,' --------- STARTING OMC ZOOM SIMULATION AND DRAWING -------------- ')

      call oc_init(pform='A4_L',psout='AdV_Layout_OMCs') ! re-initialize OPTOCAD, two PS file

      call oc_set(                     &
                lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                nice =20,           & ! Improve precision of eigenmode calculation
                dpr  = .001,         & ! default reflectivity for AR coatings
                dpt  = .999,         & ! default transmission for AR coatings
                rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                      1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                      1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                      1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                      1.507,         & ! n of BK7
                      1.94,          & ! TGG for Faraday isolators
                      2.1517,        & ! Lithium Niobate for Pockels cells
                      1.0,           & ! vacuum for diaphragms
                      1.53069/),     & ! Zerodur
                                !                mct  = 3,            & ! assume cavities in resonance
                mct  = 2,            & ! assume cavities in resonance
                ndcav= 6,            & ! number of digits to print cavity data
                pcad = 1,            & ! switch on/off printing of cavity data
                ! pctl = 0,            & ! switch on/off printing of ray-segment data while in cavity test loop
                prsd = 1,            & ! switch on/off printing of ray-segment data
                pclc = 0,            & ! switch on/off printing of lens curvatures
                pind = 1,            & ! switch on/off printing of interference data
                fslb = 20.0,          & ! character height for annotation (mm)
                cslw = .1,           & ! linewidth for surface of components (mm)
                hslc = -1,           & ! line color of hidden surfaces (-1: not plotted)
                nslc = 1) ! ,            & ! line color of neglected surfaces (1: black)
!!              write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &   !format of data to be saved in file
!               print=printPar) !format of data to be printed in stdout

      dbxof = 20
      dbyof = 15

! OMC zoom
      call oc_frame(10.89,-0.45,11.2,-0.27, dbxof,dbyof,1.,dbf, gld=.010,glc=lcg,glp=ltg) !frame
      call ps_text(dbf,'Output Mode-Cleaners',dbxs+0.55+0.02,dbys+0.11+0.02,fs=8.,ci=1) !title
    
!Trace
!      call oc_input(ocd,nib)    
!      call oc_trace(nib,save='# SIB1\_M13') ! trace all segments originating from...
!                                         ! ...beam #1 and save ray-segment data at '# SIB1\_M13'
!      call oc_reset
!      call oc_input((/'o -100., -100.'/),nib,'# SIB1\_M13') ! RCB input beam
!      call oc_trace(nib,save='# SIB1\_M8') ! trace all segments originating from...
!                                          ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
!      call oc_reset
!      call oc_input((/'o +100., +100.'/),nib,'# SIB1\_M8') ! SIB1 input beam
!      call oc_trace(nib)        ! trace all segments originating from beam #3

      call oc_set(nslc=0)       ! don't plot contours of 'n' surfaces
      do it=dbf,dbf             ! plot everything in zoom frames
         call oc_beam(2.0,fill=lcb2,lc=lcbc2,pst=pst,lw=.1,frm=it)
         call oc_beam(1.0,fill=lcb1,lc=lcbc1,pst=pst,lw=.1,frm=it)
         call oc_beam(0.0,lcb0,ltb0,.1,frm=it)
         call oc_surf(lccs,0,.1,frm=it) ! plot all surfaces
      end do

!     cavityId = 4
!     beamId = cavities(cavityId)%ir
!     call ps_msg(0,'  Beam '//trim(r2c(beamId*1.0,'F+3.0'))//': waist tests'// &
!                                trim(r2c(rs(beamId)%w(2,0)*1e6,'F+5.3'))//'  '// &
!                                trim(r2c(rs(beamId)%w(2,1)*1e6,'F+5.3'))//'  '// &
!                                trim(r2c(rs(beamId)%w(2,2)*1e6,'F+5.3')) )
!     call ps_msg(0,'  Cavity '//trim(r2c(cavityId*1.0,'F+3.0'))//': mode matchings: '// &
!                                 trim(r2c(cavities(cavityId)%mm(1),'F+2.2'))//' '//&
!                                 trim(r2c(cavities(cavityId)%mm(2),'F+2.2'))//'  input ray-segment: '//&
!                                 trim(r2c(cavities(cavityId)%ir*1.0,'F+3.0'))//' waists: '//&
!                                 trim(r2c(cavities(cavityId)%w0(1)*1e6,'F+3.3'))//'  '//&
!                                 trim(r2c(cavities(cavityId)%w0(2)*1e6,'F+3.3'))) ! //'  '//&


!goto 600
!WARNING: need to set the ray_segment structure 'public' in Optocad file and recompile Optocad for this scan !!
!For SDB1 MMT lenses optimization 
!
!    SDB1_deltaL = vector(ndL,-0.030,-0.020)
!    SDB1_L2_Roc = vector(nRocLtwo,0.6,0.80)
!    SDB1_L3_Roc = vector(nRocLthr,1.05,1.25)
!    
!    cavityId = 4    ! id of the cavity to be studied
!    OMC_range_deltaWaist = 10. ! range of plot around cavity waist (microns)
!    OMC_range_deltaDist  = 0.3 ! range of plot around distance from waist (m)
!    
!    do nn=1, ndL
!      call oc_bind('SDB1_deltaL',SDB1_deltaL(nn))
!    
!    call oc_exit(1)
!    call oc_init(plot=0,prt='')
!    call oc_set(                     &
!                dpr  = .001,         & ! default reflectivity for AR coatings
!                dpt  = .999,         & ! default transmission for AR coatings
!                rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
!                      1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
!                      1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
!                      1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
!                      1.507,         & ! n of BK7
!                      1.94,          & ! TGG for Faraday isolators
!                      2.1517,        & ! Lithium Niobate for Pockels cells
!                      1.0,           & ! vacuum for diaphragms
!                      1.53069/),     & ! Zerodur
!                mcat = 3,            & ! assume cavities in resonance
!                ndcav= 6,            &
!                pind = 1,            & ! print interference data
!                lambda=1.064e-6,     & ! light wavelength of Nd:YAG
!                fslb = 4.0,          & ! character height for annotation (mm)
!                cslw = .1,           & ! linewidth for surface of components
!                hslc = -1,           & ! line color of hidden surfaces
!                nslc = 1,            & ! line color of neglected surfaces
!                prsd = 1,            & ! switch off printing of ray-segment data
!                pcad = 1,            & ! switch off printing of cavity data
!                pclc = 0,            & ! switch off printing of lens curvatures
!                print=printPar)     !'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb')
!    
!    call ps_init
!    call ps_frame(1,SDB1_L3_Roc(1),-0.01,SDB1_L3_Roc(nRocLthr),1.01)
!    call ps_axis(1,ax='Xx',glp=0,d_tm=5.,title = 'Roc MMT\_L3 (m)')
!    call ps_axis(1,ax='Yy',glp=0,title='OMC mode-matching')
!    call ps_msg(0,'\\Loop on SDB1_MMT lenses to match OMC1: ')
!    m=1
!    do kk=1, nRocLtwo
!      call oc_bind('SDB1_L2_Roc',SDB1_L2_Roc(kk))
!      do ii=1, nRocLthr
!        call oc_bind('SDB1_L3_Roc',SDB1_L3_Roc(ii))
!
!
!     call oc_input(ocd,nib)
!     call oc_trace(nib,save='# SIB1\_M13') ! trace all segments originating from...
!                                          ! ...beam #1 and save ray-segment data at '# SIB1\_M13'!
!     call oc_input((/'o -100., -100.'/),nib,'# SIB1\_M13')     ! RCB input beam
!     call oc_trace(nib,save='# SIB1\_M8') ! trace all segments originating from...
!                                           ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
!     call oc_input((/'o +100., +100.'/),nib,'# SIB1\_M8') ! SIB1 input beam
!     call oc_trace(nib)                   ! trace all segments originating from beam #3!   
!          
!        beamId = cavities(cavityId)%ir  !id of the ray-segment hitting the cavity first
!        moma(ii,kk) =  cavities(cavityId)%mm(1) * cavities(cavityId)%mm(2)
!
!        OMC_waist = ((cavities(cavityId)%w0(1)+cavities(cavityId)%w0(2))/2.) *1e6 !microns
!
!        waist(ii,kk) = abs( (rs(beamId)%w(1,0) + rs(beamId)%w(2,0))/2.*1e6 - OMC_waist) !input beam waist (micrometer)   (expected 257 mum)
!        wsize(ii,kk) = abs( (rs(beamId)%w(1,2) + rs(beamId)%w(2,2))/2.*1e6 - OMC_waist) !size of input beam on OMC (micrometer)   (expected 257 mum)
!        zdist(ii,kk) = abs( (rs(beamId)%z(1,2) + rs(beamId)%z(2,2))/2.)                 !distance from input beam waist to OMC input (m) (expected 0 m)
!        !curv(ii,kk)  = (rs(beamId)%c(1,2) + rs(beamId)%c(2,2))/2.     !input beam curvature at OMC input
!
!        if(waist(ii,kk) < 3) then
!          call ps_msg(0,' dL = '//trim(r2c(SDB1_deltaL(nn),'F+1.3'))// &
!                        ' m, Roc MMT_L2   = '//trim(r2c(SDB1_L2_Roc(kk),'F+1.3'))// &
!                        ' m, Roc MMT_L3   = '//trim(r2c(SDB1_L3_Roc(ii),'F+1.3'))// &
!                        ' m ->  waist0 = '//trim(r2c(waist(ii,kk),'F4.1') )// &
!                        ' mu ->  wsize = '//trim(r2c(wsize(ii,kk),'F4.1') )// &
!                        ' mu  dist = '//trim(r2c(zdist(ii,kk),'F1.3') )  // &
!                        'm  w0= '//trim(r2c(OMC_waist,'F4.1') )//' mu')
!        end if
!        
!        call oc_reset(2)
!        
!      end do !end of loop on ii (Roc L3)
!
!        call ps_text(1,' {\fi2'//trim(r2c(SDB1_L2_Roc(kk),'F+1.3'))//'}\m', &
!                    xr=7.,yt=-10.-7.*m,ci=m,fs=4.)
!        call ps_plot(1,nRocLthr,SDB1_L3_Roc(:),moma(:,kk),ci=m)
!        call ps_text(1,' Mode-matching (dL='//trim(r2c(SDB1_deltaL(nn),'F4.3') )// &
!                    ')',xu=SDB1_L3_Roc(1),yu=1.1)
!        m = m+1
!
!    end do !end of loop on kk (Roc L2)
!    
!    call ps_msg(0,'End of scan on L2 and L3 focal length for a distance '//trim(r2c(SDB1_deltaL(nn),'F+1.3'))//' m:')
!    call ps_msg(0,' Range of mode-matching: '//trim(r2c(minval(moma(:,:)),'F+5.3'))//','//trim(r2c(maxval(moma(:,:)),  'F+5.3') ) )
!    call ps_msg(0,' Range of waist on OMC:  '//trim(r2c(minval(waist(:,:)),'F+5.3'))//','//trim(r2c(maxval(waist(:,:)),'F+5.3') ) )
!    call ps_msg(0,' Range of wwize on OMC:  '//trim(r2c(minval(wsize(:,:)),'F+5.3'))//','//trim(r2c(maxval(wsize(:,:)),'F+5.3') ) )
!    call ps_msg(0,' Range of dist from OMC: '//trim(r2c(minval(zdist(:,:)),'F+5.3'))// &
!       ' m,  '//trim(r2c(maxval(zdist(:,:)),'F+5.3'))// &
!       ' m')
!    
!                                !    call ps_init
!                                !    call ps_frame(1,SDB1_L2_Roc(1),SDB1_L3_Roc(1),SDB1_L2_Roc(nRocLtwo),SDB1_L3_Roc(nRocLthr),35.)
!                                !    call ps_axis(1,ax='Xx',glp=0,d_tm=5.,title='Roc MMT\_L2 (m)')
!                                !    call ps_axis(1,ax='Yy',glp=0,title='Roc MMT\_L3 (m)')
!                                !    rcl = vector(ncl,0.1,1.01)
!                                !    call ps_contour(1,nRocLtwo,nRocLthr,moma,rcl,pal=1,csw=10.)
!                                !    call ps_text(1,'OMC mode matching (dL='//trim(r2c(SDB1_deltaL(nn),'F4.3') )// &
!                                !                 ')' ,xu=SDB1_L2_Roc(1),yu=(SDB1_L3_Roc(nRoc)+0.1*(SDB1_L3_Roc(nRoc)-SDB1_L3_Roc(1))))
!    
!    call ps_init
!    call ps_frame(1,SDB1_L2_Roc(1),SDB1_L3_Roc(1),SDB1_L2_Roc(nRocLtwo),SDB1_L3_Roc(nRocLthr),35.)
!    call ps_axis(1,ax='Xx',glp=0,d_tm=5.,title='Roc MMT\_L2 (m)')
!    call ps_axis(1,ax='Yy',glp=0,title='Roc MMT\_L3 (m)')
!    rcl = vector(ncl, 0., OMC_range_deltaWaist)        !range of waists to be shown (micron, z-scale)
!    call ps_contour(1,nRocLtwo,nRocLthr,waist,rcl,pal=1,csw=10.)   
!    call ps_text(1,'Input beam waist (microns)  (dL='//trim(r2c(SDB1_deltaL(nn),'F4.3') )// &
!                 ' m)',xu=SDB1_L2_Roc(1),yu=(SDB1_L3_Roc(nRocLthr)+0.1*(SDB1_L3_Roc(nRocLthr)-SDB1_L3_Roc(1))))
!
!    call ps_init
!    call ps_frame(1,SDB1_L2_Roc(1),SDB1_L3_Roc(1),SDB1_L2_Roc(nRocLtwo),SDB1_L3_Roc(nRocLthr),35.)
!    call ps_axis(1,ax='Xx',glp=0,d_tm=5.,title='Roc MMT\_L2 (m)')
!    call ps_axis(1,ax='Yy',glp=0,title='Roc MMT\_L3 (m)')
!    rcl = vector(ncl,0.0,OMC_range_deltaDist)                                 !range of distances to be shown (m, z-scale)
!
!    call ps_contour(1,nRocLtwo,nRocLthr,zdist, rcl, pal=1,csw=10.)
!    call ps_text(1,'1.0+Dist. from OMC to waist (m) (dL='//trim(r2c(SDB1_deltaL(nn),'F4.3') )// &
!                 ' m)', xu=(SDB1_L2_Roc(1)),yu=(SDB1_L3_Roc(nRocLthr)+0.1*(SDB1_L3_Roc(nRocLthr)-SDB1_L3_Roc(1) ) ) )
!
!  end do ! end loop on nn (nDeltaL)
!    
!  call ps_msg(0,' NOTE: Nominal waist of the OMC (cavity '//trim(r2c(cavityId*1.,'F+1.0'))// &
!                '): '//trim(r2c(OMC_waist,'F+5.3'))//' microns')



!################################################################################
!##############################################################################
! New Layout (6):  Zoom on dark fringe benches squeezing benches
!##############################################################################

600 continue
!goto 700   !skip this page
    
      call ps_msg(0,' --------- STARTING DARK FRINGE BENCHES Squeezing SIMULATION AND DRAWING -------------- ')
      
      call oc_init(pform='A0_L',pid='',psout='AdV_Layout_SQBs') ! re-initialize OPTOCAD
                                !      call oc_reset
      !    pst=(/0.1,.01,.001,.0001,.00001,0.000001/)    ! Power thresholds for color steps
      
! Detection Bench Zoom (SDB1)
!      call oc_frame(dbxs,dbys,dbxe,dbye,dbxof+340,dbyof-80,0.30,dbf,gld=.025,glc=lcg,glp=ltg) !frame
!      call ps_text(dbf,'Suspended Detection Bench 1 (SDB1)',dbxs,dbys-0.10,fs=10.,ci=1) !title
!      call ps_plot(dbf,npointsSDB1,x=xsdb1+tank(1,10),y=ysdb1+tank(2,10),ci=lcben) !bench shape        
!      ! Positions of suspension fixations from Pierre Mugnier (30/04/2012)
!      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971-0.0000-0.032,-0.002-0.300-0.020,xsf=0.30,rot=0.,clip=1) !suspension fixation point
!      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971-0.2500-0.032,-0.002+0.150-0.020,xsf=0.30,rot=0.,clip=1) !suspension fixation point
!      call ps_insert(dbf,'SDB1_FixationSuspension.eps',10.971+0.2500+0.020,-0.002+0.150-0.032,xsf=0.30,rot=pi/2.,clip=1) !suspension fixation point
!      call ps_insert(dbf,'SDB1_FaradayIsolator_scale0d5.eps',11.115,0.430,xsf=2*0.30,rot=3.1416,clip=1) !Faraday isolator and mounts
!      call ps_insert(dbf,'SDB1_Telescope_2012_11.eps',10.971-0.880/2-0.025,-0.002+0.018-0.2823,xsf=0.30,rot=0.,clip=1) !Mounts of MMT
      !Positions of legs and rotation (x0,y0,a): (-0.44,0.182;-90-22.5)  (0.182,0.44;180-22.5)   (0.44,-0.182;90-22.5)   (-0.182,-0.44;-22.5)
      ! 0 is defined as the point of the leg that is in contact with the bench angle.
      !Position of figure: x = x0-Lcos(a)+lsin(a)   y=y0-Lsin(a)-lcos(a)   with L = 0.0624 m and l=0.005 m  (offset of the figure corner from 0)
!      call ps_insert(dbf,'SDB1_Leg.eps',10.550, 0.240,xsf=0.30,rot=-1.963,clip=1) !Leg to fix the bench to the marionette
!0      call ps_insert(dbf,'SDB1_Leg.eps',11.213, 0.419,xsf=0.30,rot=+2.749,clip=1) !Leg to fix the bench to the marionette
!      call ps_insert(dbf,'SDB1_Leg.eps',11.392,-0.244,xsf=0.30,rot=+1.178,clip=1) !Leg to fix the bench to the marionette
!      call ps_insert(dbf,'SDB1_Leg.eps',10.729,-0.423,xsf=0.30,rot=-0.393,clip=1) !Leg to fix the bench to the marionette
!      call ps_insert(dbf,'SDB1_TranslationStageLens.eps',10.971+0.240,-0.002+0.309,xsf=0.30,rot=pi,clip=1) !L2 translation stage
!      call ps_insert(dbf,'SDB1_TranslationStageLens.eps',10.971+0.125,-0.002+0.218,xsf=0.30,rot=-pi/2,clip=1) !L3 translation stage


!! Detection Bench Zoom (SDB2)
!      call oc_frame(db2zxs-0.10,db2zys,db2zxe+0.10,db2zye,db2zxof+100,db2zyof-20,0.30,db2f,gld=.025,glc=lcg,glp=ltg) !frame
!      call ps_text(db2f,'Suspended Detection Bench 2 (SDB2)',db2zxs,db2zye+0.05,fs=8.,ci=1) !title
!      call ps_quad(db2f,tank(1,12)-1.30/2,tank(2,12)-1.30/2,1.30,1.30,ci=lcben) !bench shape

! Suspended SQueezing Bench 1 (SQB1)
!      call oc_frame(db3zxs,db3zys,db3zxe,db3zye,db3zxof+420,db3zyof+120,0.20,db3f,gld=.025,glc=lcg,glp=ltg) !frame
      call oc_frame(db3zxs,db3zys,db3zxe,db3zye,db3zxof+420,db3zyof+120,0.20,db3f,gld=.005,glc=lcg,glp=ltg) !frame
      call ps_text(db3f,'Suspended sQueezing Bench 1 (SQB1) & External Squeezing Benches',db3zxs+0.2,db3zys-0.15,fs=10.,ci=1) !title
      call ps_quad(db3f,tank(1,28)-1.30/2,tank(2,28)-1.30/2,1.30,1.30,ci=lcben) !bench shape
      call ps_quad(db3f,tank(1,32)-1.5/2,tank(2,32)-0.75/2,1.5,0.75,ci=lcben) !bench shape
      call ps_quad(db3f,tank(1,32)+1.450-1.2/2,tank(2,32)-0.225-1.2/2,1.2,1.2,ci=lcben) !bench2 shape


!! Suspended SQueezing Bench 2 (SQB2)
      call oc_frame(db4zxs,db4zys,db4zxe,db4zye+0.5,db4zxof+25,db4zyof+140,0.20,db4f,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(db4f,'Suspended sQueezing Bench 2 (SQB2)',db4zxs+0.0,db4zys-0.1,fs=10.,ci=1) !title
      call ps_quad(db4f,tank(1,29)-1.125/2,tank(2,29)-1.565/2,1.125,1.415,ci=lcben) !bench shape

!!! Input Mirror Tower Filter Cavity (IMFC)
      call oc_frame(db5zxs,db5zys,db5zxe,db5zye,db5zxof+60,db5zyof+650,0.20,db5f,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(db5f,'Input Mirror Filter Cavity (IMFC)',db5zxs+0.0,db5zys-0.1,fs=10.,ci=1) !title
      call ps_quad(db5f,tank(1,30)-0.600/2,tank(2,30)-0.600/2,0.600,0.600,ci=lcben) !tower shape approximated as a square

!!! End Mirror Tower Filter Cavity (EMFC)
      call oc_frame(db6zxs,db6zys,db6zxe,db6zye,db6zxof+400,db6zyof+650,0.20,db6f,gld=.025,glc=lcg,glp=ltg) !frame
      call ps_text(db6f,'End Mirror Filter Cavity (EMFC)',db6zxs+0.0,db6zys-0.1,fs=10.,ci=1) !title
      call ps_quad(db6f,tank(1,31)-0.600/2,tank(2,31)-0.600/2,0.600,0.600,ci=lcben) !tower shape approximated as a square

!! External Squeezing Bench 1 (ESQB1)
!      call oc_frame(esqb1zxs,esqb1zys,esqb1zxe,esqb1zye,esqb1zxof+40,esqb1zyof+350,0.20,esqb1f,gld=.025,glc=lcg,glp=ltg) !frame
!      call ps_text(esqb1f,'External Squeezing Benches (ESQBs)',esqb1zxs,esqb1zye+0.05,fs=8.,ci=1) !title
!      call ps_quad(esqb1f,tank(1,32)-1.5/2,tank(2,32)-0.75/2,1.5,0.75,ci=lcben) !bench shape
!      call ps_quad(esqb1f,tank(1,32)+1.450-1.2/2,tank(2,32)-0.225-1.2/2,1.2,1.2,ci=lcben) !bench2 shape
!      call ps_quad(esqb1f,tank(1,28)-1.30/2,tank(2,28)-1.30/2,1.30,1.30,ci=lcben) !bench shape
      
      call oc_set(                     &
                  dpr  = .001,         &    ! default reflectivity for AR coatings
                  dpt  = .999,         & ! default transmission for AR coatings
                  rix=(/1.45003,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                        1.44963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                        1.45012,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                        1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                        1.507,         & ! n of BK7
                        1.94,          & ! TGG for Faraday isolators
                        2.1517,        & ! Lithium Niobate for Pockels cells
                        1.0,           & ! vacuum for diaphragms
                        1.53069/),     & ! Zerodur
                  mct = 3,            & ! assume cavities in resonance
                                  !                mcat = 2,            & ! assume cavities in resonance
                  ndcav= 6,            &
                                  !                pctl = 1,            & ! print data in cavity test loop
                  pind = 1,            & ! print interference data
                  lambda=1.064e-6,     & ! light wavelength of Nd:YAG
                  nice = 10,           & ! Improve precision of eigenmode calculation
                  fslb = 15.0,          & ! character height for annotation (mm)
                  cslw = .1,           & ! linewidth for surface of components
                  hslc = -1,           & ! line color of hidden surfaces
                  nslc = 1,            & ! line color of neglected surfaces
                  prsd = 1,            & ! switch off printing of ray-segment data
                  pcad = 1,            & ! switch off printing of cavity data
                  pclc = 0) !,            & ! switch off printing of lens curvatures
!                  write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6')
!                 write='x2_8 y2_8 ang_6 w2t_6 w2s_6 C2t_6 C2s_6 pw_6',  &
!                  print=printPar) !  'rs act rd ang an2 w2t w2s w0t w0s z1t_6 z2t_6 ph pw md lb')
                 
! Simulate the beams
      call oc_input(ocd,nib)
      
      do ib=1,nib
         if (ib==beamMain) then 
            call ps_msg(0,' --- SDB benches eps file: beamMain --> tracing')
            !         call oc_set(lambda=1.064e-6)
            call oc_trace(ib,save='# SIB1\_M13') ! trace all segments originating from...
                                ! ...beam #1 and save ray-segment data at '# SIB1\_M13'
            call oc_input((/'o -100., -100.'/),nib2,'# SIB1\_M13') ! RCB input beam
            call oc_trace(nib2,save='# SIB1\_M8,# SIB1\_M23') ! trace all segments originating from...
                                ! ...beam #2 and save ray-segment data at '# SIB1\_M8'
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M23') ! SIB1 input beam
            call oc_trace(nib2) ! trace all segments originating from beam #4      
            call oc_input((/'o +100., +100.'/),nib2,'# SIB1\_M8') ! SIB1 input beam
            call oc_trace(nib2)
            
            
            ! Draw the beam in the End Benches zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces    
            do it=dbf,db4f    ! plot everything in zoom frames=
               call oc_beam(3.0,fill=lcb3,lc=lcbc3,lw=.1,frm=it) !Outer part, ...
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,lw=.1,frm=it) !Middle part, ...
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,lw=.1,frm=it) !Inner part of beam
               !call oc_beam(0.0,lcb0,ltb0,.1,frm=it) !Plot beam axes in black, and no ray-segment number
               call oc_beam(0.0,lcb0,ltb0,.1,rns=2.5,rnc=2,frm=it) !Plot beam axes in black, and ray-segment numbers in red
               call oc_surf(lccs,0,.1,frm=it) ! plot all surfaces
            end do
                                ! call oc_reset
                                ! call oc_reset
            
         else                   !Other beams
            call ps_msg(0,' --- SDB benches eps file: tracing also some non-main beams')
                                !Does it properly modify the wavelength for the auxiliary beams ??  TO BE CHECKED !!!
            if (ib==beamHartmannDET) then
               call ps_msg(0,' --- SDB benches eps file: beamHartmannDET  --> 800 nm (TBC)')
               call oc_set(lambda=1064e-9)
            elseif (ib==beamGreenSQZ) then
               call ps_msg(0,' --- End benches eps file: beamGreenSQZ --> 532 nm')
               call oc_set(mct=1, rix=(/1.4607,       & ! n of Suprasil fused silica @1064nm @20 degree and 0bar
                                1.48963,       & ! n of Suprasil fused silica @1064nm @20 degree and 1bar
                                1.4607,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 0bar
                                1.44972,       & ! n of Homosil/Herasil fused silica @1064nm @20 degree and 1bar
                                1.5195,         & ! n of BK7
                                1.94,          & ! TGG for Faraday isolators
                                2.1517,        & ! Lithium Niobate for Pockels cells
                                1.0,           & ! vacuum for diaphragms
                                1.53069/),    & ! Zerodur
                                lambda=0.532e-6)
            else                !if (ib==beamAuxSNEB) then
               call ps_msg(0,' --- SDB benches eps file: skip this beam !')
               cycle
            end if
            call oc_trace(ib)   ! trace all segments originating from beam #1
                                ! Draw the beam in the BS, SPRB, PR-POP, SWEB, SNEB, NI-CP zoom frames 
            call oc_set(nslc=1) ! don't plot contours of 'n' surfaces
            do it=dbf,db6f    ! plot everything in zoom frames
               !            call oc_beam(3.0,fill=lcbaux3,lc=lcbauxc3,lw=.1,frm=it)
               call oc_beam(3.0,fill=lcb3,lc=lcbc3,lw=.1,frm=it)
               call oc_beam(2.0,fill=lcb2,lc=lcbc2,lw=.1,frm=it)
               call oc_beam(1.0,fill=lcb1,lc=lcbc1,lw=.1,frm=it)
               call oc_beam(0.0,lcb0,ltb0,.1,rns=2.5,rnc=4,frm=it) !Plot beam axes in black, and no ray-segment number
                                !call oc_beam(0.0,lcbaux0,ltbaux0,.1,rns=2.5,rnc=3,frm=it) !Plot beam axis in black, and ray-segments numbers in green
               call oc_surf(lccs,0,.1,frm=it)
            end do
            
            if (ib==beamGreenSQZ) then
            do it=dbf,db6f    ! plot everything in zoom frames
               !            call oc_beam(3.0,fill=lcbaux3,lc=lcbauxc3,lw=.1,frm=it)
!               call oc_beam(3.0,fill=lcbaux3,lc=lcbauxc3,lw=.1,frm=it)
!               call oc_beam(2.0,fill=lcbaux2,lc=lcbauxc2,lw=.1,frm=it)
 !              call oc_beam(1.0,fill=lcbaux1,lc=lcbauxc1,lw=.1,frm=it)
               call oc_beam(0.0,lcbaux0,ltb0,.1,rns=2.5,rnc=4,frm=it) !Plot beam axes in black, and no ray-segment number
                                !call oc_beam(0.0,lcbaux0,ltbaux0,.1,rns=2.5,rnc=3,frm=it) !Plot beam axis in black, and ray-segments numbers in green
               call oc_surf(lccs,0,.1,frm=it)
            end do
            end if
                                !call oc_reset
            
         end if
         
      end do
      
      ! Annotations
      call oc_frame(afxs,afys,afxe,afye,afxof-825,afyof-720,psc,af,fill=0,ax='') ! Annotations
                                !call ps_quad(af,99.,99.,13.,10.,fill=0)                              ! clean area
      call ps_insert(af,'AdV_logo.eps',102.5,100.5,xsf=0.6,clip=0) ! Virgo Logo
      call ps_text(af,'Advanced VIRGO Layout, Optocad Version \vt|'//trim(version)//  &
      '|\,, \date',xu=100.2,yu=103.5,fs=4.)
      call ps_text(af,'Romain Bonnand, Loic Rolland',xu=100.2,yu=103.2,fs=4.)
      call ps_text(af,'Layout of Output Benches (squeezing focus)',xu=100.2,yu=102.9,fs=4.)
      call oc_reset
      


!################################################################################
700 continue




!################################################################################
999 continue
    call oc_exit
    end