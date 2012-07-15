var command=environment["sun.java.command"];
var match=command.match(/ (.+)\/makesvg/);
var dir;
if (match) {
    dir = match[1];
} else {
    dir = ".";
}
print ("loading libraries from `"+dir+"/engine' ...");
load(dir+"/engine/2d.js");
load(dir+"/engine/buhin.js");
load(dir+"/engine/curve.js");
load(dir+"/engine/kage.js");
load(dir+"/engine/kagecd.js");
load(dir+"/engine/kagedf.js");
load(dir+"/engine/polygon.js");
load(dir+"/engine/polygons.js");

if(arguments.length != 1){
  print("ERROR: input the target name");
  quit();
}
target = arguments[0];

file = new java.io.File("./work");
if(!file.exists()){
  file.mkdir();
}

fis = new java.io.FileInputStream("./" + target + ".source");
isr = new java.io.InputStreamReader(fis);
br = new java.io.BufferedReader(isr);

while((line = br.readLine()) != null){
  tab = line.indexOf("\t");
  code = line.substring(0, tab);
  data = line.substring(tab + 1, line.length());
  if(data.length() > 0){
    var kage = new Kage();
    //kage.kUseCurve = true;
    kage.kUseCurve = false;
    var polygons = new Polygons();
    
    kage.kBuhin.push("temp", data + "");
    kage.makeGlyph(polygons, "temp");
    
    fos = new java.io.FileOutputStream("./work/" + code + ".svg");
    osw = new java.io.OutputStreamWriter(fos);
    bw = new java.io.BufferedWriter(osw);
    
    //bw.write(polygons.generateSVG(true));
    //bw.write(polygons.generateSVG(false));
    bw.write(polygons.generateSVGFont(false));
    
    bw.close();
    osw.close();
    fos.close();
  }
}
br.close();
isr.close();
fis.close();
