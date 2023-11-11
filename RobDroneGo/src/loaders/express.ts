import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import routes from '../api';
import config from '../../config';
import buildingRoute from "../api/routes/buildingRoute";
import robotRoute from "../api/routes/robotRoute";
import passagewayRoute from '../api/routes/passagewayRoute';
import roomRoute from '../api/routes/roomRoute';
import mapRoute from "../api/routes/mapRoute";
import userRoute from "../api/routes/userRoute";

export default ({app}: { app: express.Application }) => {
  app.enable('trust proxy');
  app.use(cors());
  app.use(require('method-override')());
  app.use(bodyParser.json());
  app.use(bodyParser.urlencoded({extended: true}));
  app.use(express.static('public'));
  app.use(config.api.prefix, routes());

  // Rota padrão para a página de login
  app.get('/', (req, res) => {
    res.redirect('/login'); // Redireciona para a página de login ("/login")
  });

  app.get('/login', (req, res) => {
    res.sendFile(__dirname + '/html/login.html'); // Envia o arquivo "login.html" para o cliente
  });

  app.post('/login', (req, res) => {
    if (req.body.email == 'admin' && req.body.password == 'admin') {
      res.redirect('/me'); // Redireciona para a página inicial ("/index"
    } else {
      res.redirect('/login'); // Redireciona para a página de login ("/login")
    }
  });

  app.get('/me', (req, res) => {
    res.sendFile(__dirname + '/html/index.html'); // Envia o arquivo "index.html" para o cliente (página inicial)
  });

  app.use('/buildings', buildingRoute());
  app.use('/robots', robotRoute());
  app.use('/passageways', passagewayRoute());
  app.use('/rooms', roomRoute());
  app.use('/maps', mapRoute());

  app.get('/status', (req, res) => {
    res.status(200).end();
  });

  app.post('/status', (req, res) => {
    if (res.status(200)) {
      console.log("status ok");
      res.end();
    } else {
      res.status(404).end();
      console.log("status not ok");
    }
  });

  // Outras rotas e middleware

  /// catch 404 and forward to error handler
  app.use((req, res, next) => {
    const err = new Error('Not Found');
    err['status'] = 404;
    next(err);
  });

  /// error handlers
  app.use((err, req, res, next) => {
    /**
     * Handle 401 thrown by express-jwt library
     */
    if (err.name === 'UnauthorizedError') {
      return res
        .status(err.status)
        .send({message: err.message})
        .end();
    }
    return next(err);
  });
  app.use((err, req, res, next) => {
    res.status(err.status || 500);
    res.json({
      errors: {
        message: err.message,
      },
    });
  });
};
