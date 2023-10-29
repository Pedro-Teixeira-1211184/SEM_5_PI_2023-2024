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
        floorID: Joi.string().required().error(new Error('Invalid floor ID'))
      })
    }), (req, res, next)  => {
      console.log("Creating a Map!");
      ctrl.createMap(req, res, next);
    }
  )
  ;

  return route;
}
