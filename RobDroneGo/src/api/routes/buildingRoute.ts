import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import IBuildingController from '../../controllers/IControllers/IBuildingController';

import config from "../../../config";

const route = Router();

export default () => {

  const ctrl = Container.get(config.controllers.building.name) as IBuildingController;

  route.post('/', celebrate({
    body: Joi.object({
      name: Joi.string().required().error(new Error('Invalid building name')),
      designation: Joi.string().required().error(new Error('Invalid building designation')),
      dimensions: Joi.string().required().error(new Error('Invalid building dimensions')),
      //description: opcional e com limite de 255 caracteres
      description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid building description')),
    })
  }), (req, res, next) => {
    console.log("Creating a Building!");
    ctrl.createBuilding(req, res, next);
  });
  return route;
}
