import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import IPassagewayController from '../../controllers/IControllers/IPassagewayController';

import config from "../../../config";

const route = Router();

export default () => {

  const ctrl = Container.get(config.controllers.passageway.name) as IPassagewayController;

  route.post('/', celebrate({
    body: Joi.object({
      floorCode1: Joi.string().required().error(new Error('Invalid floorCode1 code')),
      floorCode2: Joi.string().required().error(new Error('Invalid floorCode2 code')),
    })
  }), (req, res, next) => {
    console.log("Creating a passageway!");
    ctrl.createPassageway(req, res, next);
  });


  route.put('/', celebrate({
    body: Joi.object({
      floorCode1: Joi.string().required().error(new Error('Invalid floorID code')),
      floorCode2: Joi.string().required().error(new Error('Invalid floorID code')),
    })
  }), (req, res, next) => {
    console.log("Updating a passageway!");
    ctrl.updatePassageway(req, res, next);
  });

  route.get('/passagewaysBetweenBuildings/:buildingCode1/:buildingCode2', (req, res, next) => {
    console.log("Getting all passageways between these 2 buildings!");
    ctrl.getPassagewaysInBuildings(req, res, next);
  });

  route.get('/:floorCode', (req, res, next) => {
    console.log("Getting all passageways of these floor!");
    ctrl.getPassagewaysInFloors(req, res, next);
  });


  route.get('/', (req, res, next) => {
    console.log("Getting all passageways!");
    ctrl.getAll(req, res, next);
  });
  
  return route;
}
