import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import IRoomController from '../../controllers/IControllers/IRoomController';

import config from "../../../config";

const route = Router();

export default () => {

    const ctrl = Container.get(config.controllers.room.name) as IRoomController;

    route.post('/', celebrate({
        body: Joi.object({
            floorCode: Joi.string().required().error(new Error('Invalid floorCode code')),
            designation: Joi.string().required().error(new Error('Invalid room designation')),
            name: Joi.string().required().error(new Error('Invalid room name')),
        })
    }), (req, res, next) => {
        console.log("Creating a Room!");
        ctrl.createRoom(req, res, next);
    });

    route.get('/:floorCode', celebrate({
        params: Joi.object({
            floorCode: Joi.string().required().error(new Error('Invalid floorCode code')),
        })
    }), (req, res, next) => {
        console.log("Getting rooms of a floor!");
        ctrl.getRoomsByFloorCode(req, res, next);
    });

    return route;
}
