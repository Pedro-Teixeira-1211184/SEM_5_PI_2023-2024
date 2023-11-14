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

  route.get(
    '/authenticated',
    async (req: Request, res: Response, next: NextFunction) => {
      console.log("Checking if user is signed in!");
      ctrl.isSignedIn(req, res, next);
    }
  );

  route.post(
    '/logout',
    async (req: Request, res: Response, next: NextFunction) => {
      console.log("Signing out a User!");
      ctrl.signOut(req, res, next);
    }
  );

};
