c*************************************************
c $Id$
c
c A Fortran based non-linkimage version of succor.c. 
c
c Usage: succor inputfile [outputfile]
c
c This routine take the fully qualified name of a file written in F77
c format with the following five *required* variables, nvect (integer*4)
c followed by the four vectors U, V, Lon and Lat (real*4s all) each
c having 'nvect' elements. Additionally the file may contain information
c about the parameters needed to make the gridded field. These
c parameters must be, in order, (reals unless otherwise specified)
c lonpar(3) {min,max,inc}, latpar(3) {min,max,inc}, nreps (scalar integer*4,
c rainf(nreps) and errmax(nreps). Each variable is optional, so the user
c may write lonpar but not anything which follows. clearly if the user
c wants to write rainf he will have to write lonpar, latpar and nreps.
c
c This program reads the contents of the file, creates the arrays needed
c to call objanl.f, calls that subroutine, gathers its output and writes
c the interpolated field to a file named "./succor-out.bin" unless an
c alternative file is specified as the second parameter on the command line.
c
c Input: fully qualified name of input file.
c Outputs: 
c          stdout: name of output file
c          stderr: 'succor: ERROR, error message'
c
c Returns: 0 on success, 1 on failure
c
c This routine writes error messages, all of which are prefaced with the
c string 'succor: ERROR ' to standard error.
c
c
c Modification Log:
c
c $Log$
c Revision 1.1  2002/05/06 15:43:32  vapdev
c objanl2.f, succorf.f: new all fortran versions of the succor.
c
c
c*************************************************
      program succorf
      implicit none
      integer*4 MAXVEC/500000/, MAXLON/1440/, MAXLAT/720/
c
c     --- maximum number of vectors = 400,000
c     --- maximum grid quantization 1/12 of a degree
c
      character*256 inputfile, outputfile, prognam
      character*256 use1/'Usage: succor infile outfile'/

      integer*4 STDERR/0/,STDOUT/6/, iblank, oblank
      integer*4 INFILE/22/,OUTFILE/23/

C -------------- Local Variables -----------------

c     Read from input file
      real*4 u(300000),v(300000),lon(300000),lat(300000)

c     aux arrays based on input data
      real*4 rinfo(6,300000), udev(300000),vdev(300000),
     + alat(300000) !,time(300000)
      
c     arrays based on grid dimensions
      real*4 a(3,1440,720), vg(1440,720), ug(1440,720),
     + work(1440,720),
     + sumx(1440, 720), sumy(1440, 720)

      integer*4 store(1440,720)
      real*4 time/1.0/
      real*4 errmax(5)/5*50/
      real*4 rainf(5)/8.,5,3,2,1/
      
      real*4 lonmin/0/, lonmax/359/, loninc/1/
      real*4 latmin/-60/, latmax/60/, latinc/1/
      real*4 sp/37676.0/
      real*4 gamma(9)/9*1.0/
      real*4 denom(3)/0.25, -0.5, 1/
      real*4 lonpar(3), latpar(3)
     
      integer*4 i,j,k,dummy, first, argc, blank
      integer*4 nreps/5/, nvesc/1/, nvect/-1/, l, m
      character*256 argv, str
      integer*4 iargc, ios



      argc=iargc()
      if (argc.lt.2) then 
         print*,use1
      endif 

      call getarg(0, prognam)
      call getarg(1, inputfile)
      if (argc.gt.1) then 
         call getarg(2, outputfile)
      else 
         outputfile = './succor-out.bin'
      endif 

c      print *,"program: "//prognam(1:len(prognam)-1)
      iblank=index(inputfile,' ')
      oblank=index(outputfile,' ')
      write( STDOUT,*) prognam//" input file: "//
     +        inputfile(1:iblank)
      write( STDOUT,*) "output file: "//
     +        outputfile(1:oblank)

      open(OUTFILE,file=outputfile(1:oblank),access='sequential',
     +     iostat=ios, status='new',form='unformatted')
      if (ios.ne.0) then 
         blank = index(prognam,' ')
         str = prognam(1:blank)//':ERROR opening output file,'
     +     //outputfile(1:oblank)//' ios = '
         blank=index(str,'=')
         write( STDERR,*) str(1:blank+1),ios
c 20      format(a80, i)
         call exit(1)
      endif 


      open(INFILE,file=inputfile,access='sequential',
     +      iostat=ios, status='old',form='unformatted')

      if (ios.ne.0) then
         write (STDERR,*) prognam//':ERROR, opening '//inputfile
         call exit(1)
      endif 
      
      
         
      read(INFILE) nvect
      if (nvect.le.0.or.nvect.gt.MAXVEC) then 
         str=prognam//':ERROR, Bad NVECT!, nvect = '
         blank=index(str,' = ')
         write( STDERR,*) str(1:blank+1), nvect
         call exit(1)
      endif 

         read(INFILE) (u(i), i=1,nvect), (v(i), i=1,nvect), 
     +               (lon(i), i=1,nvect), (lat(i), i=1,nvect)
         read(INFILE,iostat=ios, end=10) lonmin
         read(INFILE,iostat=ios, end=10) lonmax
         read(INFILE,iostat=ios, end=10) loninc
         read(INFILE,iostat=ios, end=10) latmin
         read(INFILE,iostat=ios, end=10) latmax
         read(INFILE,iostat=ios, end=10) latinc
         read(INFILE,iostat=ios, end=10) nreps
         read(INFILE,iostat=ios, end=10) (rainf(i),i=1,nreps)
         read(INFILE,iostat=ios, end=10) (errmax(i), i=1,nreps)
 10      continue
         if (nreps.lt.5) then 
            do i=nreps+1,5 
               rainf(i)=1
               errmax(i)=1.e5
            end do
         endif 
         
         close(INFILE)


         lonpar(1)=lonmin
         lonpar(2)=lonmax
         lonpar(3)=loninc
         
         latpar(1)=latmin
         latpar(2)=latmax
         latpar(3)=latinc
         
         l = (lonmax-lonmin)/loninc+1
         m = (latmax-latmin)/latinc+1
         
         do i=1,nvect
            rinfo(1,i) = 1.
            rinfo(2,i) = lat(i)
            rinfo(3,i) = lon(i)
            rinfo(4,i) = time
            rinfo(5,i) = u(i)
            rinfo(6,i) = v(i)
         end do
         
         call objanl2( sp,rinfo,ug,vg,l,m,nvect,nreps,time,
     1        sumx,sumy,a,store,latmin,lonmin,latinc,
     2        loninc,nvesc,rainf, gamma,denom,errmax,rainf,
     3        udev,vdev,alat,work, MAXLON, MAXLAT)
         
         
         write(OUTFILE) l,m
         write(OUTFILE) ((ug(i,j),i=1,l), j=1,m)
         write(OUTFILE) ((vg(i,j),i=1,l), j=1,m)
         write(OUTFILE) lonpar
         write(OUTFILE) latpar
         close(OUTFILE)
         call exit(0)
            
      end


