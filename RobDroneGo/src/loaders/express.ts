import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import routes from '../api';
import config from '../../config';
import User from "../persistence/schemas/userSchema";

export default ({app}: { app: express.Application }) => {
    app.enable('trust proxy');
    app.use(cors());
    app.use(require('method-override')());
    app.use(bodyParser.json());
    app.use(express.static('public'));
    app.use(config.api.prefix, routes());

    // Rota padrão para a página de login
    app.get('/', (req, res) => {
        res.redirect('/login'); // Redireciona para a página de login ("/login")
    });

    app.get('/login', (req, res) => {
        res.sendFile(__dirname + '/html/login.html'); // Envia o arquivo "login.html" para o cliente
    });

    // Rota para a página do usuário após o login bem-sucedido
    app.get('/userPage', (req, res) => {
        // Certifique-se de que o arquivo "userPage.html" esteja no local correto
        res.sendFile(__dirname + '/html/userPage.html');
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
