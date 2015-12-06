/**
Codigo de Ejemplo Dado por Mpineda, para video Juega de Space Invaders.
 * Descripción
Monta el lado del Servidor, y entrega parametros de un get en formato json, 
se debe convertir esto con html para mostrarlo bonito.Para mas informacion ve 
al sitio de node.js
 * @method Nombre de la función

 */
var express = require('express');
var app = express();

var bodyParser   = require('body-parser');
var cookieParser = require('cookie-parser');

app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(cookieParser());

var state = [];

app.get('/', function(req, res){
  res.json(state);
});


app.post('/', function(req, res) {
  console.log(req.body);
  state.push(req.body);
  res.json({'ok': true});
});

app.listen(9999);
