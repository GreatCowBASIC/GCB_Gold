#gawk -f convert16F180xx.awk -v CHIP=PIC16F18075 PPStoModule.txt 16F171xxPPSInputSelectionTable.txt  16F180xxPPSOutput.txt pic16f180xx_40pinTemplate_description.xml
#gawk -f convert16F180xx.awk -v CHIP=PIC16F18054 PPStoModule.txt 16F171xxPPSInputSelectionTable.txt  16F180xxPPSOutput.txt pic16f180xx_28pinTemplate_description.xml

BEGIN {
FS=","
modulearraypointer = 0
outputarraypointer = 0
templatepointer = 0
pinAliaspointer = 0
printf( "Output filename %s\n", CHIP )

}
{
  #Load the file
  if ( toupper(FILENAME) == "PPSTOMODULE.TXT" ) {
    newline = trim($0)
    if ( substr( newline, 1 , 1 ) == "'" ) {
      next
    }
    pps = $1
    module = $2
    modules[ modulearraypointer, 1 ]=pps
    modules[ modulearraypointer, 2 ]=module
    #print "modules  " modules[ modulearraypointer, 1 ] " + " modules[ modulearraypointer, 2 ]
    modulearraypointer++
    next
    }

  #read and load the INPUTPPS
  if ( toupper(FILENAME) == "16F171XXPPSINPUTSELECTIONTABLE.TXT" ) {
    newline = trim($0)
    if ( substr( newline, 1 , 1 ) == "'" ) {
      next
    }
    module = $1
    pps = $2
    ppsinput[ outputarraypointer, 1 ]=pps
    ppsinput[ outputarraypointer, 2 ]=module
    outputarraypointer++
    next
  }

  #read and load the template
  if ( index(toupper(FILENAME), "TEMPLATE_DESCRIPTION.XML" ) != 0 ) {
    newline = trim($0)
    if ( substr( newline, 1 , 1 ) == "'" ) {
      next
    }
    inline = $0
    if ( (index( inline , "CHIPNAME") ) != 0 ) {
      sub( "\"CHIPNAME\"", "\""CHIP"\"", inline )
    }
    template[ templatepointer]=inline
    templatepointer++
    next
  }


  newline = trim($0)
  if ( substr( newline, 1 , 1 ) == "'" )
    next
  if ( index( toupper($0), "RESERVED" ) != 0 )
    next
  pinAliases[trim($2)]=   sprintf ("            <pinAlias alias=\"%s\" module=\"%s\" value=\"%s\" direction=\"output\">",$2,matchPPStoModule($2),$1)
  pinAlias++
}

END {
  outfile = CHIP"_description.XML"
  outfile = tolower(outfile)
  print outfile
  lineone = 0
  x = 0
  for (x=0; x<templatepointer;x++) {
    if ( trim(template[x]) != "" ) {

    #find a valid pps output line
      if ( index( template[x], "direction=\"output" ) != 0 ) {
        # <pinAlias alias="ADGRDB" module="ADCC" value="0x25" direction="output">

        #extract the alias name from the template
        findPinAlias = substr( template[x],index(template[x], "\"")+1,255)
        findPinAlias = substr( findPinAlias, 1, index(findPinAlias, " ")-2)

        #lookup array entry
        if ( trim(pinAliases[trim(findPinAlias)]) != "" ) {
          print "Matched Alias " findPinAlias
          print pinAliases[trim(findPinAlias)] >> outfile
          #a match, so, delete from the array called pinAliases
          delete pinAliases[trim(findPinAlias)]

          }
        else {
          #cycle thru until next definition
        print "Additive - rename/remove ? " findPinAlias  " -  " trim(template[x])
          do {
            x++
          } while ( index( template[x] ,"</pinAlias>")==0 )


          #print "<pinAlias alias=\"UNKNOWN\">"  >> outfile
        }
      }
      else {
        if ( lineone==0) {
          print template[x] > outfile
          lineone = 1
        }
        else
          print template[x] >> outfile
      }
    }
  }

  for ( x in pinAliases )
    if ( trim(pinAliases[x]) != "" ){

      print "Not resolved " pinAliases[x]
    }
}


function ltrim(s) { sub(/^[ \t\r\n]+/, "", s); return s }
function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
function trim(s)  { return rtrim(ltrim(s)); }

function matchPPStoModule ( s ) {

  for ( x=0;x<modulearraypointer;x++ ) {

    if ( trim(s) == trim(modules[ x , 1 ]) ) {
      return modules[ x , 2 ]
    }
  }

  return "Error"
}
