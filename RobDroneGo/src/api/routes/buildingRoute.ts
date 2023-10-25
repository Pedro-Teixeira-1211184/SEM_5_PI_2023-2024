import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import IBuildingController from '../../controllers/IControllers/IBuildingController';
import IFloorController from '../../controllers/IControllers/IFloorController';

import config from "../../../config";

const route = Router();

export default () => {

    const ctrl = Container.get(config.controllers.building.name) as IBuildingController;
    const floorCtrl = Container.get(config.controllers.floor.name) as IFloorController;

    route.post('/', celebrate({
        body: Joi.object({
            code: Joi.string().required().max(5).error(new Error('Invalid building code')),
            dimensions: Joi.string().required().error(new Error('Invalid building dimensions')),
            name: Joi.string().max(50).allow('').allow(null).error(new Error('Invalid building name')),
            //description: opcional e com limite de 255 caracteres
            description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid building description')),
            maxFloors: Joi.number().integer().required().error(new Error('Invalid building maxFloors')),
            minFloors: Joi.number().integer().required().error(new Error('Invalid building minFloors'))
        })
    }), (req, res, next) => {
        console.log("Creating a Building!");
        ctrl.createBuilding(req, res, next);
    });

    route.post('/floors', celebrate({
        body: Joi.object({
            buildingCode: Joi.string().required().max(5).error(new Error('Invalid building code')),
            number: Joi.string().required().error(new Error('Invalid floor number')),
            description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid floor description'))
        })
    }), (req, res, next) => {
        console.log("Creating a Floor!");
        floorCtrl.createFloor(req, res, next);
    });

    route.put('/', celebrate({
        body: Joi.object({
            code: Joi.string().required().max(5).error(new Error('Invalid building code')),
            //dimensions: obrigatório e deve ser do tipo string composta por (número)x(número)
            dimensions: Joi.string().required().error(new Error('Invalid building dimensions')),
            name: Joi.string().max(50).allow('').allow(null).error(new Error('Invalid building name')),
            //description: opcional e com limite de 255 caracteres
            description: Joi.string().max(255).allow('').allow(null).error(new Error('Invalid building description')),
            maxFloors: Joi.number().integer().required().error(new Error('Invalid building maxFloors')),
            minFloors: Joi.number().integer().required().error(new Error('Invalid building minFloors'))
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
