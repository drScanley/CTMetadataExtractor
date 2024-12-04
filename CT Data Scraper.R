#'@export
#'Extract CT metadata and specimen information from files and create a Morphosource Batch manifest
#'@param DIRECTORY path to the directory with your files.
#'@param Institution Darwin core code of the institution where your specimens are from.
#'@param CollectionCode Darwin core Code of the collection where your specimens are from.
#'@param Creator Individual or individuals that produced the original data
#'@param Scanning.system Brand of CT scanner used to produce metadata files if \code{"Bruker"} Bruker log files. if \code{"Nikon"} Nikon xtekct files, if \code{"Phoenix"}Phoenix .pca files
#'@param Delimiter characters used to split Darwin core Triplets. if \code{D1} = spaces,hyphens and underscores, if \code{D2} = Spaces and underscores if \code{D3} = Underscores and hyphens, if \code{D4} = spaces and hyphens)
#'@param Name.system How to parse file names if \code{"3Part"} = Inst:Col:SpecNo (e.g.UF-Herp-12345) if \code{"2Part"} = INST:SpecNo (e.g. UF-12345)
#'@param AddAccessionFirst Included if the specimen number requires a leading qualifier (e.g. AddAccessionFirst="R" = MCZ_herps_R12345)
#'@param AddAccessionLast Included if the specimen number requires a trailing qualifier (e.g. AddAccessionLast="-frog" = MCZ_herps_12345-frog)
#'@param media_pub_status "Private":"Restricted download":"Open download"
#'@param media_type pre-set type of data
#'@param media_raw_or_derived whether the media / zip files contains raw— i.e. radiographs— or derived —ie tomograms, mesh files, videos etc
#'@return a formatted xlsx file  for Morphosource batch upload
#'@export
#'@examples
#'MSMetadataBatch<-function(DIRECTORY="UF",Institution="UF", CollectionCode="herp",Creator="Edward Stanley",Scanning.system="Phoenix",Delimiter="D1",Name.system="3Part",AddAccession="",media_pub_status="private",media_type="CTImageSeries",media_raw_or_derived="derived")

# setwd("/Volumes/DIL/CTData2/Chordata/Reptilia/Lepidosaura/")


CTMetadataScan<-function(DIRECTORY,Scanning.system="Phoenix", exposure="Unreported",Creator = "USER",  imaging_event.ct.xray_tube_type = "none", imaging_event.ct.target_type = "reflection",   imaging_event.ct.detector_type = " Scintillator (Phosphor used)", imaging_event.ct.detector_configuration ="Area (single or tiled detector)", imaging_event.ct.target_material="tungston")
{
  require(ridigbio)
  require(stringr)
  require(readr)
  require(xlsx)
  #remember current working directory
  mypath <- getwd()

 

  ###
  ######
  ### If scanning on a Phoenix system with PCA metadata files
  if(Scanning.system=="Phoenix"){
  
  ###

    #get the names and number of files in DIRECTORY for loop

    list.files(path = DIRECTORY,pattern = ".pca",recursive = TRUE)-> pcalist
    #remove any .link files that will screw things up
    pcalist[!grepl(".lnk",pcalist)]->pcalist
    length(pcalist)->metadatafileNo
    #make a blank data frame to be filled in by loop
    Matrix1<-data.frame()
    for (i in 1:metadatafileNo){

      #set up text reporting for loop
      Sys.sleep(0.1)
      round(i/metadatafileNo*100,digits=2)->P

      #Get file in DIRECTORY
      pcalist[i]->DWCTrip
      #report which file is being processed
      message(paste(P,"%","parsing",DWCTrip, sep=" "))
      #Split the pca name and file directory
      as.list(unlist(strsplit(DWCTrip, split="/")))->iDWCTList
      tail(iDWCTList,n=1)->iPCAName
      iPCAName->iPCANameOriginal
      gsub(iPCAName,"",DWCTrip)->iDirectory

   
      
      AccessionNo<-"none"
      CollectionCode<-"none"
      InstitutionCode<-"none"
      
 
      #open pca file and read data into relevant objects
      paste(DIRECTORY,pcalist[i],sep = "/")->PCApath
      gsub("//","/",PCApath)->PCApath
      conn<-file(PCApath,open="r")
      linn<-readLines(conn)
      close(conn)
      linn[grep('^Voxel.*', linn)]->Voxelsize
      Voxelsize<-gsub("VoxelSizeX=","",as.character(Voxelsize))
      Voxelsize<-gsub("Voxelsize=","",as.character(Voxelsize))
      Voxelsize<-as.numeric(Voxelsize[1])
      linn[grep('^NumberImages*', linn)]->ImageNo
      ImageNo<-gsub("NumberImages=","",as.character(ImageNo))
      ImageNo<-as.numeric(ImageNo[1])
      linn[grep('^Voltage=*', linn)]->Voltage
      Voltage<-gsub("Voltage=","",as.character(Voltage))
      Voltage<-as.numeric(Voltage)

      linn[grep('^Current=*', linn)]->Current
      Current<-gsub("Current=","",as.character(Current))
      Current<-as.numeric(Current)

      gsub("XRayFilter","Filter",linn)->linn
      linn[grep('^Filter=*', linn)]->Filter
      Filter<-gsub("Filter=","",as.character(Filter))

      if(length(Filter)==2){
      Filter<-gsub("FilterThickness=","",as.character(Filter))
      paste(Filter[1],Filter[2])->Filter}

      if(Filter=="Unknown"){
        Filter<-"None"
      }
      if(Filter=="Please define a filter."){
        Filter<-"None"
      }
      if(Filter=="Filter="){
        Filter<-"None"
      }
      linn[grep('^Avg=*', linn)]->Avg
      Avg<-gsub("Avg=","",as.character(Avg))
      Avg<-as.numeric(Avg[2])

      linn[grep('^TimingVal=*', linn)]->TimingVal
      TimingVal<-gsub("TimingVal=","",as.character(TimingVal))
      TimingVal<-as.numeric(TimingVal)

      linn[grep('^FDD=*', linn)]->FDD
      FDD<-gsub("FDD=","",as.character(FDD))
      FDD<-as.numeric(FDD)

      linn[grep('^FOD=*', linn)]->FOD
      FOD<-gsub("FOD=","",as.character(FOD))
      FOD<-as.numeric(FOD)

      linn[grep('^PixelsizeX=*', linn)]->PixelsizeX
      PixelsizeX<-gsub("PixelsizeX=","",as.character(PixelsizeX))
      PixelsizeX<-as.numeric(PixelsizeX)
      linn[grep('^NrPixelsX=*', linn)]->NrPixelsX
      NrPixelsX<-gsub("NrPixelsX=","",as.character(NrPixelsX))
      NrPixelsX<-as.numeric(NrPixelsX)
      linn[grep('^PixelsizeY=*', linn)]->PixelsizeY
      PixelsizeY<-gsub("PixelsizeY=","",as.character(PixelsizeY))
      PixelsizeY<-as.numeric(PixelsizeY)
      linn[grep('^NrPixelsY=*', linn)]->NrPixelsY
      NrPixelsY<-gsub("NrPixelsY=","",as.character(NrPixelsY))
      NrPixelsY<-as.numeric(NrPixelsY)
      (Voltage*Current/1000000)->Wattage
      
      paste(DIRECTORY,DWCTrip,sep = "/")->PCAlocation
      gsub("//","/",PCAlocation)->PCAlocation
      file.info(PCAlocation)->FileInfo
      FileInfo$ctime->DateCreated

      #bind all data into a row
      Dataline <-cbind(iPCAName,PCAlocation,  as.character(DateCreated), Voxelsize, Voxelsize, Voxelsize, Voxelsize, TimingVal, Filter, Avg, ImageNo, Voltage, Wattage, Current, imaging_event.ct.xray_tube_type, imaging_event.ct.target_type, imaging_event.ct.detector_type, NrPixelsX, PixelsizeX, NrPixelsY, PixelsizeY, imaging_event.ct.detector_configuration, FOD, FDD, imaging_event.ct.target_material)
      

      #Append row to dataframe Matrix1
      Matrix1<-rbind(Matrix1,Dataline)
    }


}



if(Scanning.system=="Nikon"){
  
  ###
  
  #get the names and number of files in DIRECTORY for loop
  
  list.files(path = DIRECTORY,pattern = ".xtekct")-> Xtekctlist
  length(Xtekctlist)->metadatafileNo
  #make a blank data frame to be filled in by loop
  Matrix1<-data.frame()
  for (i in 1:metadatafileNo){
    
    #set up text reporting for loop
    Sys.sleep(0.1)
    round(i/metadatafileNo*100,digits=2)->P
    
    #Get file in DIRECTORY
    Xtekctlist[i]->DWCTrip
    #report which file is being processed
    message(paste(P,"%","parsing",DWCTrip, sep=" "))
    #Split the pca name and file directory
    as.list(unlist(strsplit(DWCTrip, split="/")))->iDWCTList
    tail(iDWCTList,n=1)->iPCAName
    iPCAName->iPCANameOriginal
    gsub(iPCAName,"",DWCTrip)->iDirectory
    
    
    
    AccessionNo<-"none"
    CollectionCode<-"none"
    InstitutionCode<-"none"
    
    
    #open xtekct file and read data into relevant objects
    paste(DIRECTORY,Xtekctlist[i],sep = "/")->Xtekctpath
    conn<-file(Xtekctpath,open="r")
    linn<-readLines(conn)
    close(conn)
    linn[grep('^VoxelSizeX=*', linn)]->Voxelsize
    Voxelsize<-readr::parse_number(Voxelsize)
    linn[grep('^Projections*', linn)]->ImageNo
    ImageNo<-readr::parse_number(ImageNo)
    linn[grep('^XraykV=*', linn)]->Voltage
    Voltage<-readr::parse_number(Voltage)
    
    linn[grep('^XrayuA=*', linn)]->Current
    Current<-readr::parse_number(Current)
    
    linn[grep('^Filter_ThicknessMM=*', linn)]->Filter
    Filter<-gsub("Filter_ThicknessMM=","",as.character(Filter))
    if(Filter=="Unknown"){
      Filter<-"None"
    }
    if(Filter=="Please define a filter."){
      Filter<-"None"
    }
    
    linn[grep('^TimingVal=*', linn)]->TimingVal
    TimingVal<-readr::parse_number(TimingVal)
    TimingVal<-exposure
    
    linn[grep('^SrcToDetector=*', linn)]->FDD
    FDD<-readr::parse_number(FDD)
    
    linn[grep('^SrcToObject=*', linn)]->FOD
    FOD<-readr::parse_number(FOD)
    
    linn[grep('^DetectorPixelSizeX=*', linn)]->PixelsizeX
    PixelsizeX<-readr::parse_number(PixelsizeX)
    linn[grep('^DetectorPixelsX=*', linn)]->NrPixelsX
    NrPixelsX<-readr::parse_number(NrPixelsX)
    linn[grep('^DetectorPixelSizeY=*', linn)]->PixelsizeY
    PixelsizeY<-readr::parse_number(PixelsizeY)
    linn[grep('^DetectorPixelsY=*', linn)]->NrPixelsY
    NrPixelsY<-readr::parse_number(NrPixelsY)
    (Voltage*Current/1000000)->Wattage
    
    
    paste(DIRECTORY,DWCTrip,sep = "/")->PCAlocation
    gsub("//","/",PCAlocation)->PCAlocation
    file.info(PCAlocation)->FileInfo
    FileInfo$ctime->DateCreated
    
    #bind all data into a row
    Dataline <-cbind(iPCAName,PCAlocation,  as.character(DateCreated), Voxelsize, Voxelsize, Voxelsize, Voxelsize, TimingVal, Filter, Avg, ImageNo, Voltage, Wattage, Current, imaging_event.ct.xray_tube_type, imaging_event.ct.target_type, imaging_event.ct.detector_type, NrPixelsX, PixelsizeX, NrPixelsY, PixelsizeY, imaging_event.ct.detector_configuration, FOD, FDD, imaging_event.ct.target_material)
    
    
    #Append row to dataframe Matrix1
    Matrix1<-rbind(Matrix1,Dataline)
  }
  
  
}
  
  
  if(Scanning.system=="Bruker"){
    
    ###
    
    #get the names and number of files in DIRECTORY for loop
    
    list.files(path = DIRECTORY,pattern = ".log")-> Loglist
    length(Loglist)->metadatafileNo
    #make a blank data frame to be filled in by loop
    Matrix1<-data.frame()
    for (i in 1:metadatafileNo){
      
      #set up text reporting for loop
      Sys.sleep(0.1)
      round(i/metadatafileNo*100,digits=2)->P
      
      #Get file in DIRECTORY
      pcalist[i]->DWCTrip
      #report which file is being processed
      message(paste(P,"%","parsing",DWCTrip, sep=" "))
      #Split the pca name and file directory
      as.list(unlist(strsplit(DWCTrip, split="/")))->iDWCTList
      tail(iDWCTList,n=1)->iPCAName
      iPCAName->iPCANameOriginal
      gsub(iPCAName,"",DWCTrip)->iDirectory
      
      
      
      AccessionNo<-"none"
      CollectionCode<-"none"
      InstitutionCode<-"none"
      
      
      #open xtekct file and read data into relevant objects
      paste(DIRECTORY,Xtekctlist[i],sep = "/")->Xtekctpath
      conn<-file(Xtekctpath,open="r")
      linn<-readLines(conn)
      close(conn)
      linn[grep('^VoxelSizeX=*', linn)]->Voxelsize
      Voxelsize<-readr::parse_number(Voxelsize)
      linn[grep('^Projections*', linn)]->ImageNo
      ImageNo<-readr::parse_number(ImageNo)
      linn[grep('^XraykV=*', linn)]->Voltage
      Voltage<-readr::parse_number(Voltage)
      
      linn[grep('^XrayuA=*', linn)]->Current
      Current<-readr::parse_number(Current)
      
      linn[grep('^Filter_ThicknessMM=*', linn)]->Filter
      Filter<-gsub("Filter_ThicknessMM=","",as.character(Filter))
      if(Filter=="Unknown"){
        Filter<-"None"
      }
      if(Filter=="Please define a filter."){
        Filter<-"None"
      }
      
      linn[grep('^TimingVal=*', linn)]->TimingVal
      TimingVal<-readr::parse_number(TimingVal)
      TimingVal<-exposure
      
      linn[grep('^SrcToDetector=*', linn)]->FDD
      FDD<-readr::parse_number(FDD)
      
      linn[grep('^SrcToObject=*', linn)]->FOD
      FOD<-readr::parse_number(FOD)
      
      linn[grep('^DetectorPixelSizeX=*', linn)]->PixelsizeX
      PixelsizeX<-readr::parse_number(PixelsizeX)
      linn[grep('^DetectorPixelsX=*', linn)]->NrPixelsX
      NrPixelsX<-readr::parse_number(NrPixelsX)
      linn[grep('^DetectorPixelSizeY=*', linn)]->PixelsizeY
      PixelsizeY<-readr::parse_number(PixelsizeY)
      linn[grep('^DetectorPixelsY=*', linn)]->NrPixelsY
      NrPixelsY<-readr::parse_number(NrPixelsY)
      (Voltage*Current/1000000)->Wattage
      
      
      paste(DIRECTORY,DWCTrip,sep = "/")->PCAlocation
      gsub("//","/",PCAlocation)->PCAlocation
      file.info(PCAlocation)->FileInfo
      FileInfo$ctime->DateCreated
      
      #bind all data into a row
      Dataline <-cbind(iPCAName,PCAlocation,  as.character(DateCreated), Voxelsize, Voxelsize, Voxelsize, Voxelsize,TimingVal, Filter, Avg, ImageNo, Voltage, Wattage, Current, imaging_event.ct.xray_tube_type, imaging_event.ct.target_type, imaging_event.ct.detector_type, NrPixelsX, PixelsizeX, NrPixelsY, PixelsizeY, imaging_event.ct.detector_configuration, FOD, FDD, imaging_event.ct.target_material)
      
      
      #Append row to dataframe Matrix1
      Matrix1<-rbind(Matrix1,Dataline)
    }
    
    
  }
  
  
  #rename matrix1 collumns
  colnames(Matrix1)<-c("Metadata file name", "Metadata file location", "Scan Date", "Voxel x spacing", "Voxel y spacing", "Voxel z spacing", "slice thickness","Detector capture time", "filter", "frame averaging", "ct.projections", "voltage", "power", "amperage", "xray tube type", "target type", ".detector type", "detector pixels number x", "detector pixel size x", "detector pixels number y", "detector pixel size y", "detector configuration", "source object distance", "source detector distance", "X-ray tube target material")
  
  
  
  #go to DIRECTORY, save CSV and return to original directory
  gsub("/","",as.character(DIRECTORY))->DIRFolder
  paste(DIRFolder,"All CT Scan List.xlsx",sep = " ")->fileName
  
  write.xlsx(Matrix1,file = fileName,row.names = FALSE)
  setwd(mypath)
  return(Matrix1)
}
