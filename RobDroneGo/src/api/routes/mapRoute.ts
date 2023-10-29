import {Router} from 'express';


import {Container} from 'typedi';

import config from "../../../config";
import IBuildingController from "../../controllers/IControllers/IBuildingController";
import IMapController from "../../controllers/IControllers/IMapController";

const route = Router();

export default () => {

  const ctrl = Container.get(config.controllers.map.name) as IMapController;

  route.patch('/', (req, res, next) => {
    console.log("Creating a Map!");
    ctrl.createMap(req, res, next);
  });

  return route;
}
