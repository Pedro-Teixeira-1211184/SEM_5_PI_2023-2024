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
      code: Joi.string().required().max(5).error(new Error('Invalid building code')),
      //dimensions: obrigatório e deve ser do tipo string composta por (número)x(número)
      dimensions: Joi.string().required().error(new Error('Invalid building dimensions')),
      name: Joi.string().max(50).allow('').allow(null).error(new Error('Invalid building name')),
      //description: opcional e com limite de 255 caracteres
      description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid building description')),
    })
  }), (req, res, next) => {
    console.log("Creating a Building!");
    ctrl.createBuilding(req, res, next);
  });

  route.put('/', celebrate({
    body: Joi.object({
      code: Joi.string().required().max(5).error(new Error('Invalid building code')),
      //dimensions: obrigatório e deve ser do tipo string composta por (número)x(número)
      dimensions: Joi.string().required().error(new Error('Invalid building dimensions')),
      name: Joi.string().max(50).allow('').allow(null).error(new Error('Invalid building name')),
      //description: opcional e com limite de 255 caracteres
      description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid building description')),
    })
  }), (req, res, next) => {
    console.log("Updating a Building!");
    ctrl.updateBuilding(req, res, next);
  });

  route.get('/', (req, res, next) => {
    console.log("Getting all Buildings!");
    ctrl.getAll(req, res, next);
  });

  return route;
}
