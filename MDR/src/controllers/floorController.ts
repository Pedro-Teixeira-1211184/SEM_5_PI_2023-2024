import {Inject, Service} from 'typedi';
import {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IBuildingService from "../services/IServices/IBuildingService";
import IFloorController from "./IControllers/IFloorController";
// @ts-ignore
import {StatusCodes} from "http-status-codes";
import IFloorService from "../services/IServices/IFloorService";
import IFloorDTO from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import IBuildingDTO from "../dto/IBuildingDTO";
import IPassagewayService from "../services/IServices/IPassagewayService";
import IPassagewayDTO from '../dto/IPassagewayDTO';

@Service()
export default class FloorController implements IFloorController /* TODO: extends ../core/infra/BaseController */ {
    constructor(
        @Inject(config.services.floor.name) private floorServiceInstance: IFloorService,
        @Inject(config.services.building.name) private buildingServiceInstance: IBuildingService,
        @Inject(config.services.passageway.name) private passagewayServiceInstance: IPassagewayService
    ) {
    }

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            //check if building exists by buildingCode
            const buildingOrError = await this.buildingServiceInstance.getBuildingByCode(req.body.buildingCode) as Result<IBuildingDTO>;

            if (buildingOrError.isFailure) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(buildingOrError.errorValue());
            }

            //check if floor number is valid
            if (Number(req.body.number) > buildingOrError.getValue().maxFloors) {
                console.log("Floor number is invalid!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Floor number is higher than maximum number of floors!");
            }

            const floorOrError = await this.floorServiceInstance.createFloor(req.body as IFloorDTO) as Result<IFloorDTO>;
            if (floorOrError.isFailure) {
                console.log("Floor not created!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(floorOrError.errorValue());
            }

            const floorResult = floorOrError.getValue();
            return res.status(StatusCodes.CREATED).json(floorResult);
        } catch (e) {
            return next(e);
        }
    }

    public async findFloorsByBuildingCode(req: Request, res: Response, next: NextFunction) {
        try {
            const buildingOrError = await this.buildingServiceInstance.getBuildingByCode(req.params.buildingCode) as Result<IBuildingDTO>;
            if (buildingOrError.isFailure) {
                console.log("Building not found!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Building not found!");
            }
            const floorOrError = await this.floorServiceInstance.findFloorsByBuildingCode(req.params.buildingCode) as Result<IFloorDTO[]>;
            if (floorOrError.isFailure) {
                console.log("Floor not found!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("No floors found!");
            }

            const floorResult = floorOrError.getValue();
            return res.status(StatusCodes.OK).json(floorResult);
        } catch (e) {
            return next(e);
        }
    }

    public async findFloorsByPassageways(req: Request, res: Response, next: NextFunction) {
        try {
            const floorOrError = await this.floorServiceInstance.findFloorsByBuildingCode(req.params.buildingCode) as Result<IFloorDTO[]>;
            if (floorOrError.isFailure) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send("No floors found!");
            }
            const floorsInBuilding = floorOrError.getValue();

            const getFloorsWithPassageway: IFloorDTO[] = [];

            for ( let i = 0; i < floorsInBuilding.length; i++) {
                const boolean = await this.passagewayServiceInstance.findFloorsInPassageways(floorsInBuilding[i].code);
                if (boolean.isSuccess) {
                    getFloorsWithPassageway.push(floorsInBuilding[i]);
                }
            }

            if (getFloorsWithPassageway.length == 0) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send("No passageways found in building!");
            }

    
            const passagewaysInBuilding: IPassagewayDTO[] = [];

            for (let i = 0; i < getFloorsWithPassageway.length; i++) {
                const aux = await this.passagewayServiceInstance.getPassagewaysInFloors(getFloorsWithPassageway[i].code) as Result<IPassagewayDTO[]>;
                const aux1 = aux.getValue();
                if (aux1.length != 0) {
                    for ( let k = 0; k < aux1.length; k++){
                        passagewaysInBuilding.push(aux1[k]);
                    }
                }
            }

            let buildingsWithPassageWay: string[] = [];

            for (let i = 0; i < passagewaysInBuilding.length; i++) {
                const aux2 = passagewaysInBuilding[i].floorCode1;
                const aux3 = passagewaysInBuilding[i].floorCode2;
                if ( aux2.substring(0, 1) == req.params.buildingCode && !buildingsWithPassageWay.includes(aux3.substring(0, 1))) {
                    buildingsWithPassageWay.push(aux3.substring(0, 1));
                }

                if ( aux3.substring(0, 1) == req.params.buildingCode && !buildingsWithPassageWay.includes(aux2.substring(0, 1))) {
                    buildingsWithPassageWay.push(aux2.substring(0, 1));
                }
            }
            let result: string[] = [];
            for (let i = 0; i < getFloorsWithPassageway.length; i++) {
                result.push("Building Code:"+ getFloorsWithPassageway[i].buildingCode + " Number:"+ getFloorsWithPassageway[i].number + " Floor Code:"+getFloorsWithPassageway[i].code + " Description:"+ getFloorsWithPassageway[i].description);
            }

            result = result.concat("Buildings connected to: " + buildingsWithPassageWay);
            
            for (let i = 0; i < result.length; i++) {
                result[i].split("\n");
            }

            return res.status(StatusCodes.OK).json(result);
        } catch (e) {
            return next(e);
        }
    }

    public async updateFloor(req: Request, res: Response, next: NextFunction) {
    try{
        const floorOrError = await this.floorServiceInstance.updateFloor(req.body as IFloorDTO) as Result<IFloorDTO>;
        if (floorOrError.isFailure) {
            return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(floorOrError.errorValue());
        }

        const floorDTO = floorOrError.getValue();
        return res.json(floorDTO).status(StatusCodes.ACCEPTED);
    }catch(e){
        return next(e);
    }
    }

    public async getAll(req: Request, res: Response, next: NextFunction) {
        try {
            const floorOrError = await this.floorServiceInstance.getAll() as Result<IFloorDTO[]>;
            if (floorOrError.isFailure) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(floorOrError.errorValue());
            }

            const floorDTO = floorOrError.getValue();
            return res.json(floorDTO).status(StatusCodes.ACCEPTED);
        } catch (e) {
            return next(e);
        }
    }

}

