import {Router} from 'express';


import {Container} from 'typedi';

import config from "../../../config";
import IMapController from "../../controllers/IControllers/IMapController";
import {celebrate, Joi} from "celebrate";

const route = Router();

export default () => {

  const ctrl = Container.get(config.controllers.map.name) as IMapController;

  route.patch('/', celebrate({
      body: Joi.object({
        buildingCode: Joi.string().required().error(new Error('Invalid building code')),
        floorNumber: Joi.number().required().error(new Error('Invalid floor number')),
        size: Joi.object({
          height: Joi.number().required().error(new Error('Invalid length')),
          width: Joi.number().required().error(new Error('Invalid width')),
        }).required().error(new Error('Invalid size')),
        map: Joi.array().items(Joi.array().items(Joi.number())).required().error(new Error('Invalid map')),
        rooms: Joi.array().items(Joi.object({
          name: Joi.string().required().error(new Error('Invalid room name')),
          dimensions: Joi.object({
            top: Joi.object({
              x: Joi.number().required().error(new Error('Invalid top x')),
              y: Joi.number().required().error(new Error('Invalid top y')),
            }).required().error(new Error('Invalid top')),
            bottom: Joi.object({
              x: Joi.number().required().error(new Error('Invalid bottom x')),
              y: Joi.number().required().error(new Error('Invalid bottom y')),
            }).required().error(new Error('Invalid bottom')),
          }).required().error(new Error('Invalid room dimensions')),
          door: Joi.object({
            coordinates: Joi.object({
              x: Joi.number().required().error(new Error('Invalid door x')),
              y: Joi.number().required().error(new Error('Invalid door y')),
            }).required().error(new Error('Invalid door coordinates')),
            orientation: Joi.string().required().error(new Error('Invalid door orientation')),
          }).required().error(new Error('Invalid room door')),
        })).error(new Error('Invalid rooms')),
        passageways: Joi.array().items(Joi.object({
          start: Joi.string().required().error(new Error('Invalid passageway start')),
          end: Joi.string().required().error(new Error('Invalid passageway end')),
          localization: Joi.object({
            coordinates: Joi.object({
              x: Joi.number().required().error(new Error('Invalid passageway x')),
              y: Joi.number().required().error(new Error('Invalid passageway y')),
            }).required().error(new Error('Invalid passageway coordinates')),
            orientation: Joi.string().required().error(new Error('Invalid passageway orientation')),
          }).required().error(new Error('Invalid passageway localization')),
        })).error(new Error('Invalid passageways')),
        elevator: Joi.array().items(Joi.object({
          localization: Joi.object({
            coordinates: Joi.object({
              x: Joi.number().required().error(new Error('Invalid elevator x')),
              y: Joi.number().required().error(new Error('Invalid elevator y')),
            }).required().error(new Error('Invalid elevator coordinates')),
            orientation: Joi.string().required().error(new Error('Invalid elevator orientation'))
          }).required().error(new Error('Invalid elevator localization')),
        })).error(new Error('Invalid elevator')),
      }).required().error(new Error('Invalid map')),
    }), (req, res, next) => {
      console.log("Creating a Map!");
      ctrl.createMap(req, res, next);
    }
  )
  ;

  route.get('/', (req, res, next) => {
    console.log("Getting all Maps!");
    ctrl.getAll(req, res, next);
  });

  route.get('/allPlants', (req, res, next) => {
    console.log("Getting all Plants!");
    ctrl.getAllPlants(req, res, next);
  });

  route.get('/:buildingCode/:floorNumber', (req, res, next) => {
    console.log("Loading a Map!");
    ctrl.loadMap(req, res, next);
  });

  route.get('/list', (req, res, next) => {
    console.log("Listing Maps!");
    ctrl.listMaps(req, res, next);
  });

  route.get('/path/:origin/:destination', (req, res, next) => {
    console.log("Finding path between floors!");
    ctrl.pathBetweenFloors(req, res, next);
  });

  return route;
}
