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
          width: Joi.number().required().error(new Error('Invalid width')),
          height: Joi.number().required().error(new Error('Invalid height')),
        }).required().error(new Error('Invalid size')),
        map: Joi.array().items(Joi.array().items(Joi.number())).required().error(new Error('Invalid map')),
        rooms: Joi.array().items(Joi.object({
          name: Joi.string().required().error(new Error('Invalid room name')),
          dimensions: Joi.string().required().error(new Error('Invalid room dimensions')),
          door: Joi.string().required().error(new Error('Invalid room door')),
        })).error(new Error('Invalid rooms')),
        passageways: Joi.array().items(Joi.object({
          start: Joi.string().required().error(new Error('Invalid passageway start')),
          end: Joi.string().required().error(new Error('Invalid passageway end')),
          localization: Joi.string().required().error(new Error('Invalid passageway localization')),
        })).error(new Error('Invalid passageways')),
        elevator: Joi.object({
          localization: Joi.string().required().error(new Error('Invalid elevator localization')),
        }).error(new Error('Invalid elevator'))
      })
    }), (req, res, next)  => {
      console.log("Creating a Map!");
      ctrl.createMap(req, res, next);
    }
  )
  ;

  return route;
}
