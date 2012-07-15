function Kage(size){
  // method
  function makeGlyph(polygons, buhin){ // void
    var glyphData = this.kBuhin.search(buhin);
    if(glyphData != ""){
      this.drawStrokesArray(polygons, this.adjustKirikuchi(this.adjustUroko2(this.adjustUroko(this.adjustKakato(this.adjustTate(this.adjustMage(this.getEachStrokes(glyphData))))))));
    }
  }
  Kage.prototype.makeGlyph = makeGlyph;
  
  function getEachStrokes(glyphData){ // strokes array
    var strokesArray = new Array();
    var strokes = glyphData.split("$");
    for(var i = 0; i < strokes.length; i++){
      var columns = strokes[i].split(":");
      if(Math.floor(columns[0]) != 99){
        strokesArray.push([
          Math.floor(columns[0]),
          Math.floor(columns[1]),
          Math.floor(columns[2]),
          Math.floor(columns[3]),
          Math.floor(columns[4]),
          Math.floor(columns[5]),
          Math.floor(columns[6]),
          Math.floor(columns[7]),
          Math.floor(columns[8]),
          Math.floor(columns[9]),
          Math.floor(columns[10])
          ]);
      } else {
        var buhin = this.kBuhin.search(columns[7]);
        if(buhin != ""){
          strokesArray = strokesArray.concat(this.getEachStrokesOfBuhin(buhin,
                                                  Math.floor(columns[3]),
                                                  Math.floor(columns[4]),
                                                  Math.floor(columns[5]),
                                                  Math.floor(columns[6]))
                            );
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.getEachStrokes = getEachStrokes;
  
  function getEachStrokesOfBuhin(buhin, x1, y1, x2, y2){
    var temp = this.getEachStrokes(buhin);
    var result = new Array();
    for(var i = 0; i < temp.length; i++){
      result.push([temp[i][0],
                   temp[i][1],
                   temp[i][2],
                   x1 + temp[i][3] * (x2 - x1) / 200,
                   y1 + temp[i][4] * (y2 - y1) / 200,
                   x1 + temp[i][5] * (x2 - x1) / 200,
                   y1 + temp[i][6] * (y2 - y1) / 200,
                   x1 + temp[i][7] * (x2 - x1) / 200,
                   y1 + temp[i][8] * (y2 - y1) / 200,
                   x1 + temp[i][9] * (x2 - x1) / 200,
                   y1 + temp[i][10] * (y2 - y1) / 200]);
    }
    return result;
  }
  Kage.prototype.getEachStrokesOfBuhin = getEachStrokesOfBuhin;
  
  function adjustUroko(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if(strokesArray[i][0] == 1 && strokesArray[i][2] == 0){ // no operation for TATE
        for(var k = 0; k < this.kAdjustUrokoLengthStep; k++){
          var tx, ty, tlen;
          if(strokesArray[i][4] == strokesArray[i][6]){ // YOKO
            tx = strokesArray[i][5] - this.kAdjustUrokoLine[k];
            ty = strokesArray[i][6] - 0.5;
            tlen = strokesArray[i][5] - strokesArray[i][3];
          } else {
            var rad = Math.atan((strokesArray[i][6] - strokesArray[i][4]) / (strokesArray[i][5] - strokesArray[i][3]));
            tx = strokesArray[i][5] - this.kAdjustUrokoLine[k] * Math.cos(rad) - 0.5 * Math.sin(rad);
            ty = strokesArray[i][6] - this.kAdjustUrokoLine[k] * Math.sin(rad) - 0.5 * Math.cos(rad);
            tlen = Math.sqrt((strokesArray[i][6] - strokesArray[i][4]) * (strokesArray[i][6] - strokesArray[i][4]) +
                             (strokesArray[i][5] - strokesArray[i][3]) * (strokesArray[i][5] - strokesArray[i][3]));
          }
          if(tlen < this.kAdjustUrokoLength[k] ||
             isCrossWithOthers(strokesArray, i, tx, ty, strokesArray[i][5], strokesArray[i][6])
             ){
            strokesArray[i][2] += (this.kAdjustUrokoLengthStep - k) * 100;
            k = Infinity;
          }
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustUroko = adjustUroko;
  
  function adjustUroko2(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if(strokesArray[i][0] == 1 && strokesArray[i][2] == 0 && strokesArray[i][4] == strokesArray[i][6]){
        var pressure = 0;
        for(var j = 0; j < strokesArray.length; j++){
          if(i != j && (
             (strokesArray[j][0] == 1 && strokesArray[j][4] == strokesArray[j][6] &&
              !(strokesArray[i][3] + 1 > strokesArray[j][5] || strokesArray[i][5] - 1 < strokesArray[j][3]) &&
              Math.abs(strokesArray[i][4] - strokesArray[j][4]) < this.kAdjustUroko2Length) ||
             (strokesArray[j][0] == 3 && strokesArray[j][6] == strokesArray[j][8] &&
              !(strokesArray[i][3] + 1 > strokesArray[j][7] || strokesArray[i][5] - 1 < strokesArray[j][5]) &&
              Math.abs(strokesArray[i][4] - strokesArray[j][6]) < this.kAdjustUroko2Length)
             )){
            pressure += Math.pow(this.kAdjustUroko2Length - Math.abs(strokesArray[i][4] - strokesArray[j][6]), 1.1);
          }
        }
        var result = Math.min(Math.floor(pressure / this.kAdjustUroko2Length), this.kAdjustUroko2Step) * 100;
        if(strokesArray[i][2] < result){
          strokesArray[i][2] = strokesArray[i][2] % 100 + Math.min(Math.floor(pressure / this.kAdjustUroko2Length), this.kAdjustUroko2Step) * 100;
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustUroko2 = adjustUroko2;
  
  function adjustTate(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if((strokesArray[i][0] == 1 || strokesArray[i][0] == 3 || strokesArray[i][0] == 7) && strokesArray[i][3] == strokesArray[i][5]){
        for(var j = 0; j < strokesArray.length; j++){
          if(i != j && (strokesArray[j][0] == 1 || strokesArray[j][0] == 3 || strokesArray[j][0] == 7) && strokesArray[j][3] == strokesArray[j][5] &&
             !(strokesArray[i][4] + 1 > strokesArray[j][6] || strokesArray[i][6] - 1 < strokesArray[j][4]) &&
             Math.abs(strokesArray[i][3] - strokesArray[j][3]) < this.kMinWidthT * this.kAdjustTateStep){
            strokesArray[i][1] += (this.kAdjustTateStep - Math.floor(Math.abs(strokesArray[i][3] - strokesArray[j][3]) / this.kMinWidthT)) * 1000;
            if(strokesArray[i][1] > this.kAdjustTateStep * 1000){
              strokesArray[i][1] = strokesArray[i][1] % 1000 + this.kAdjustTateStep * 1000;
            }
          }
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustTate = adjustTate;
  
  function adjustMage(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if((strokesArray[i][0] == 3) && strokesArray[i][6] == strokesArray[i][8]){
        for(var j = 0; j < strokesArray.length; j++){
          if(i != j && (
             (strokesArray[j][0] == 1 && strokesArray[j][4] == strokesArray[j][6] &&
              !(strokesArray[i][5] + 1 > strokesArray[j][5] || strokesArray[i][7] - 1 < strokesArray[j][3]) &&
              Math.abs(strokesArray[i][6] - strokesArray[j][4]) < this.kMinWidthT * this.kAdjustMageStep) ||
             (strokesArray[j][0] == 3 && strokesArray[j][6] == strokesArray[j][8] &&
              !(strokesArray[i][5] + 1 > strokesArray[j][7] || strokesArray[i][7] - 1 < strokesArray[j][5]) &&
              Math.abs(strokesArray[i][6] - strokesArray[j][6]) < this.kMinWidthT * this.kAdjustMageStep)
             )){
            strokesArray[i][2] += (this.kAdjustMageStep - Math.floor(Math.abs(strokesArray[i][6] - strokesArray[j][6]) / this.kMinWidthT)) * 1000;
            if(strokesArray[i][2] > this.kAdjustMageStep * 1000){
              strokesArray[i][2] = strokesArray[i][2] % 1000 + this.kAdjustMageStep * 1000;
            }
          }
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustMage = adjustMage;
  
  function adjustKirikuchi(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if(strokesArray[i][0] == 2 && strokesArray[i][1] == 32 &&
         strokesArray[i][3] > strokesArray[i][5] &&
         strokesArray[i][4] < strokesArray[i][6]){
        for(var j = 0; j < strokesArray.length; j++){ // no need to skip when i == j
          if(strokesArray[j][0] == 1 &&
             strokesArray[j][3] < strokesArray[i][3] && strokesArray[j][5] > strokesArray[i][3] &&
             strokesArray[j][4] == strokesArray[i][4] && strokesArray[j][4] == strokesArray[j][6]){
            strokesArray[i][1] = 132;
            j = strokesArray.length;
          }
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustKirikuchi = adjustKirikuchi;
  
  function adjustKakato(strokesArray){ // strokesArray
    for(var i = 0; i < strokesArray.length; i++){
      if(strokesArray[i][0] == 1 &&
         (strokesArray[i][2] == 13 || strokesArray[i][2] == 23)){
        for(var k = 0; k < this.kAdjustKakatoStep; k++){
          if(isCrossBoxWithOthers(strokesArray, i,
                               strokesArray[i][5] - this.kAdjustKakatoRangeX / 2,
                               strokesArray[i][6] + this.kAdjustKakatoRangeY[k],
                               strokesArray[i][5] + this.kAdjustKakatoRangeX / 2,
                               strokesArray[i][6] + this.kAdjustKakatoRangeY[k + 1])
             | strokesArray[i][6] + this.kAdjustKakatoRangeY[k + 1] > 200 // adjust for baseline
             | strokesArray[i][6] - strokesArray[i][4] < this.kAdjustKakatoRangeY[k + 1] // for thin box
             ){
            strokesArray[i][2] += (3 - k) * 100;
            k = Infinity;
          }
        }
      }
    }
    return strokesArray;
  }
  Kage.prototype.adjustKakato = adjustKakato;
  
  function drawStrokesArray(polygons, strokesArray){
    for(var i = 0; i < strokesArray.length; i++){
      dfDrawFont(this, polygons,
                 strokesArray[i][0],
                 strokesArray[i][1],
                 strokesArray[i][2],
                 strokesArray[i][3],
                 strokesArray[i][4],
                 strokesArray[i][5],
                 strokesArray[i][6],
                 strokesArray[i][7],
                 strokesArray[i][8],
                 strokesArray[i][9],
                 strokesArray[i][10]);
    }
  }
  Kage.prototype.drawStrokesArray = drawStrokesArray;
  
  function drawGlyph(polygons, glyph){ // void
    // [glyph] : [stroke]$[stroke]$.....
    // [stroke] : [column]:[column]:.....
    var strokes = glyph.split("$");
    for(var i = 0; i < strokes.length; i++){
      var columns = strokes[i].split(":");
      if(Math.floor(columns[0]) != 99){
        dfDrawFont(this, polygons,
                   Math.floor(columns[0]),
                   Math.floor(columns[1]), Math.floor(columns[2]),
                   Math.floor(columns[3]), Math.floor(columns[4]),
                   Math.floor(columns[5]), Math.floor(columns[6]),
                   Math.floor(columns[7]), Math.floor(columns[8]),
                   Math.floor(columns[9]), Math.floor(columns[10]));
      } else {
        var buhin = this.kBuhin.search(columns[7]);
        if(buhin != ""){
          this.drawBuhin(polygons, buhin,
                         Math.floor(columns[3]),
                         Math.floor(columns[4]),
                         Math.floor(columns[5]),
                         Math.floor(columns[6]));
        }
      }
    }
  }
  Kage.prototype.drawGlyph = drawGlyph;
  
  function drawBuhin(polygons, glyph, x1, y1, x2, y2){ // void
    var strokes = glyph.split("$");
    for(var i = 0; i < strokes.length; i++){
      var columns = strokes[i].split(":");
      if(Math.floor(columns[0]) != 99){
        dfDrawFont(this, polygons,
                   Math.floor(columns[0]),
                   Math.floor(columns[1]),
                   Math.floor(columns[2]),
                   x1 + Math.floor(columns[3]) * (x2 - x1) / 200,
                   y1 + Math.floor(columns[4]) * (y2 - y1) / 200,
                   x1 + Math.floor(columns[5]) * (x2 - x1) / 200,
                   y1 + Math.floor(columns[6]) * (y2 - y1) / 200,
                   x1 + Math.floor(columns[7]) * (x2 - x1) / 200,
                   y1 + Math.floor(columns[8]) * (y2 - y1) / 200,
                   x1 + Math.floor(columns[9]) * (x2 - x1) / 200,
                   y1 + Math.floor(columns[10]) * (y2 - y1) / 200);
      } else {
        var buhin = this.kBuhin.search(columns[7]);
        if(buhin != ""){
          this.drawBuhin(polygons, buhin,
                         x1 + Math.floor(columns[3]) * (x2 - x1) / 200,
                         y1 + Math.floor(columns[4]) * (y2 - y1) / 200,
                         x1 + Math.floor(columns[5]) * (x2 - x1) / 200,
                         y1 + Math.floor(columns[6]) * (y2 - y1) / 200);
        }
      }
    }
  }
  Kage.prototype.drawBuhin = drawBuhin;
  
  //properties
  Kage.prototype.kMincho = 0;
  Kage.prototype.kGothic = 1;
  this.kShotai = this.kMincho;
  
  this.kRate = 100;
  
  if(size == 1){
    this.kMinWidthY = 1.2;
    this.kMinWidthT = 3.6;
    this.kWidth = 3;
    this.kKakato = 1.8;
    this.kL2RDfatten = 1.1;
    this.kMage = 6;
    this.kUseCurve = 0;
    
    this.kAdjustKakatoL = ([8, 5, 3, 1]); // for KAKATO adjustment 000,100,200,300
    this.kAdjustKakatoR = ([4, 3, 2, 1]); // for KAKATO adjustment 000,100,200,300
    this.kAdjustKakatoRangeX = 12; // check area width
    this.kAdjustKakatoRangeY = ([1, 11, 14, 18]); // 3 steps of checking
    this.kAdjustKakatoStep = 3; // number of steps
    
    this.kAdjustUrokoX = ([14, 12, 9, 7]); // for UROKO adjustment 000,100,200,300
    this.kAdjustUrokoY = ([7, 6, 5, 4]); // for UROKO adjustment 000,100,200,300
    this.kAdjustUrokoLength = ([13, 21, 30]); // length for checking
    this.kAdjustUrokoLengthStep = 3; // number of steps
    this.kAdjustUrokoLine = ([13, 15, 18]); // check for crossing. corresponds to length
  } else {
    this.kMinWidthY = 2;
    this.kMinWidthT = 6;
    this.kWidth = 5;
    this.kKakato = 3;
    this.kL2RDfatten = 1.1;
    this.kMage = 10;
    this.kUseCurve = 0;
    
    this.kAdjustKakatoL = ([14, 9, 5, 2]); // for KAKATO adjustment 000,100,200,300
    this.kAdjustKakatoR = ([8, 6, 4, 2]); // for KAKATO adjustment 000,100,200,300
    this.kAdjustKakatoRangeX = 20; // check area width
    this.kAdjustKakatoRangeY = ([1, 19, 24, 30]); // 3 steps of checking
    this.kAdjustKakatoStep = 3; // number of steps
    
    this.kAdjustUrokoX = ([24, 20, 16, 12]); // for UROKO adjustment 000,100,200,300
    this.kAdjustUrokoY = ([12, 11, 9, 8]); // for UROKO adjustment 000,100,200,300
    this.kAdjustUrokoLength = ([22, 36, 50]); // length for checking
    this.kAdjustUrokoLengthStep = 3; // number of steps
    this.kAdjustUrokoLine = ([22, 26, 30]); // check for crossing. corresponds to length
    
    this.kAdjustUroko2Step = 3;
    this.kAdjustUroko2Length = 40;
    
    this.kAdjustTateStep = 4;
    
    this.kAdjustMageStep = 5;
  }
  
  this.kBuhin = new Buhin();
  
  return this;
}
