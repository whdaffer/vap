/*
 * Adapted from 'subobj.f' which I got from Victor Zlotniki.
 * This subroutine is based (loosely) on the paper 
 *
 *  "Low-Level Flows of the GATE area during Summer 1972"
 *   Gregory j. Tripoli and T.N.Krishnamurti
 *   Dept of Meteorology, Florida State University, Tallahassee Fla
 *   
 *  Monthly Weather Review, Volume 103
 *  pp 197-216 
 *  March 1975.
 * 
 * $Id$
 *
 * Modification Log
 *
 * $Log$
 *
 * Usage: status=objanl(sp,rinfo,ug,vg,l,m,nvect,nreps,time,
 *                      sumx,sumy,a,store,latpar,lonpar,nvesc,trainf,
 *                      gamma,denom,ermax,rainf,udev,vdev,alat,work)
 *
 * I   sp:special (bad) value if(rinfo(5,*),(6,*) = sp) vector not used.
 * I,O rinfo(1,kk)=1.
 * I,O rinfo(2,kk)=latitude    (O: y grid coordinate)
 * I,O rinfo(3,kk)=longitude   (O: x grid coordinate)
 * I,O rinfo(4,kk)=time        (O: time weight)
 * I,O rinfo(5,kk)=1st dimension of data.
 * I,O rinfo(6,kk)=2nd dimension of data.  (if (nvesc.eq.1))
 * I,O ug, vg: Guess fields.
 * I,O l,m: 
 * I,O nvect: number of points in rinfo.  (O: n.retained vectors).
 * I   nreps: number of iterations throught the *big* loop.
 * I,O time
 * I,O sumx, sumy
 * I,O a
 * I,O store
 * I,O rltmn,rlnmn
 * I,O rltinc,rlninc
 * I,O nvesc : Flag, if data is 2-dimensional, rinfo(6,*) has the second dimension.
 * I,O trainf
 * I,O pres
 * I,O gamma
 * I,O denom
 * I,O ermx:  Error for each iteration of big loop. Data larger than
 *            this is rejected.
 * I,O rainf: spatial search radii, one per iteration, in gridstep units.
 * I,O udev, vdev
 * I,O alat
 * I,O lt,mt
 *
 *
 *
 */

int objanl(
