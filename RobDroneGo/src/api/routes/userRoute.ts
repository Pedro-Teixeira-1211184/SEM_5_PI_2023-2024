import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';

import middlewares from '../middlewares';
import { celebrate, Joi } from 'celebrate';
import winston = require('winston');
import config from "../../../config";
import IUserController from '../../controllers/IControllers/IUserController';

const route = Router();

export default (app: Router) => {
  const ctrl = Container.get(config.controllers.user.name) as IUserController;

  app.use('/auth', route);

  route.post(
    '/signup',
    celebrate({
      body: Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        email: Joi.string().required(),
        password: Joi.string().required(),
        role: Joi.string().required()
      }),
    }),
    async (req: Request, res: Response, next: NextFunction) => {
      console.log("Creating a User!");
      ctrl.signUp(req, res, next);
    }
  );

  route.post(
    '/signin',
    celebrate({
      body: Joi.object({
        email: Joi.string().required(),
        password: Joi.string().required(),
      }),
    }),
    async (req: Request, res: Response, next: NextFunction) => {
        console.log("Signing in a User!");
        ctrl.signIn(req, res, next);
    },
  );

  /**
   * @TODO Let's leave this as a place holder for now
   * The reason for a logout route could be deleting a 'push notification token'
   * so the device stops receiving push notifications after logout.
   *
   * Another use case for advance/enterprise apps, you can store a record of the jwt token
   * emitted for the session and add it to a black list.
   * It's really annoying to develop that but if you had to, please use Redis as your data store
   */
  route.post('/logout', middlewares.isAuth, (req: Request, res: Response, next: NextFunction) => {
    const logger = Container.get('logger') as winston.Logger;
    logger.debug('Calling Sign-Out endpoint with body: %o', req.body)
    try {
      //@TODO AuthService.Logout(req.user) do some clever stuff
      return res.status(200).end();
    } catch (e) {
      logger.error('🔥 error %o', e);
      return next(e);
    }
  });

};
