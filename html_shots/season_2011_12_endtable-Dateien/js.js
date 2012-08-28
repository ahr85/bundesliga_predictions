function Klapp(z, schalter){
 if(schalter == 1)
 {
  document.getElementById(z).style.display = 'block';
 } else {
  document.getElementById(z).style.display = 'none';
 }
}

function Nachladen(z, schalter, inhalt){
 if(schalter == 1)
 {
  document.getElementById(z).style.display = 'block';
 } else {
  document.getElementById(z).style.display = 'none';
 }
}


function ladeSaison(){
  liga = document.getElementById('liga').value*1;
  
  document.getElementById('saison').length=0;
  for (var WIDSID in LigaV[liga]){
    var saison = LigaV[liga][WIDSID].split("|")[0];
    s = new Option(saison, WIDSID,false,true);
    document.getElementById('saison').options[document.getElementById('saison').options.length] = s;
  }
  document.getElementById('saison').options[0].selected=true;
}

function ladeVerein(){
  liga = document.getElementById('liga').value*1;
  
  document.getElementById('mid').length=0;
  
  if (liga==0) {
    for(var i = 1; i <= 4; i++){
      for (var MID in LigaV_M[i]){
        var Verein = LigaV_M[i][MID];
        s = new Option(Verein, MID,false,true);
        document.getElementById('mid').options[document.getElementById('mid').options.length] = s;
      }
    }
  } else {
    for (var MID in LigaV_M[liga]){
      var Verein = LigaV_M[liga][MID];
      s = new Option(Verein, MID,false,true);
      document.getElementById('mid').options[document.getElementById('mid').options.length] = s;
    }
  }
  document.getElementById('mid').options[0].selected=true;
}

function ladeSpTag(){
  var liga = document.getElementById('liga').value;
  var saison = document.getElementById('saison').value;
  //alert(LigaV[liga][saison].split("|")[0]);
  var von =LigaV[liga][saison].split("|")[1];
  var bis =LigaV[liga][saison].split("|")[2];
  
  document.getElementById('sptag').length=0;
  for(var i = bis; i >= von; i--){
    s = new Option(i, i,false,true);
    document.getElementById('sptag').options[document.getElementById('sptag').options.length] = s;
  }
  document.getElementById('sptag').options[0].selected=true;
}
