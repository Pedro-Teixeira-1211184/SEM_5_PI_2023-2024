import {Router, Request, Response, NextFunction} from 'express';
import {Container} from 'typedi';

import middlewares from '../middlewares';
import {celebrate, Joi} from 'celebrate';
import winston = require('winston');
import config from "../../../config";
import IUserController from '../../controllers/IControllers/IUserController';

const route = Router();

export default (app: Router) => {
  const ctrl = Container.get(config.controllers.user.name) as IUserController;

  app.use('/auth', route);

  route.post('/request', celebrate({
      body: Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        nif: Joi.string().required(),
        email: Joi.string().required(),
        password: Joi.string().required()
      }),
    }), async (req: Request, res: Response, next: NextFunction) => {
      console.log("Creating a User Request!");
      ctrl.signUpRequest(req, res, next);
    }
  );

  route.get('/request', async (req: Request, res: Response, next: NextFunction) => {
      console.log("Getting all User Requests!");
      ctrl.getAllUserRequests(req, res, next);
    }
  );

  route.delete('/request/:email', async (req: Request, res: Response, next: NextFunction) => {
      console.log("Deleting a User Request!");
      ctrl.deleteUserRequest(req, res, next);
    }
  );

  route.post(
    '/signup',
    celebrate({
      body: Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        nif: Joi.string().required(),
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

  route.delete('/delete/:email', async (req: Request, res: Response, next: NextFunction) => {
    console.log("Deleting a User!");
    ctrl.deleteUser(req, res, next);
  });

  route.get('/:email', async (req: Request, res: Response, next: NextFunction) => {
    console.log("Getting a User!");
    ctrl.getUser(req, res, next);
  });

  route.put('/:email', async (req: Request, res: Response, next: NextFunction) => {
    console.log("Updating a User!");
    ctrl.updateUser(req, res, next);
  });

};
