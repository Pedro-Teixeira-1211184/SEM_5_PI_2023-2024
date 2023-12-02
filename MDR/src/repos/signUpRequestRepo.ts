import {Service, Inject} from 'typedi';

import ISignUpRequestRepo from "../services/IRepos/ISignUpRequestRepo";
import {SignUpRequest} from "../domain/signUpRequest";
import {UserEmail} from "../domain/userEmail";
import {SignUpRequestMap} from "../mappers/SignUpRequestMap";
import {Document, FilterQuery, Model} from "mongoose";
import {ISignUpRequestPersistence} from "../dataschema/ISignUpRequestPersistence";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";


@Service()
export default class SignUpRequestRepo implements ISignUpRequestRepo {

    constructor(
        @Inject('signUpRequestSchema') private signUpResquestSchema: Model<ISignUpRequestPersistence & Document>,
    ) {
    }

    delete(signUpRequest: SignUpRequest): Promise<void> {
        return Promise.resolve(undefined);
    }

    public async exists(signUpRequest: SignUpRequest): Promise<boolean> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            const query = {signUpRequestEmail: signUpRequest.email};
            const floorDocument = await this.signUpResquestSchema.findOne(query as FilterQuery<IFloorPersistence & Document>);
            return floorDocument != null;
        } catch (error) {
            throw error;
        }
    }

    findByEmail(email: UserEmail): Promise<SignUpRequest> {
        return Promise.resolve(undefined);
    }

    public async save(signUpRequest: SignUpRequest): Promise<SignUpRequest> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            if (!await this.exists(signUpRequest)) {
                const pers = SignUpRequestMap.toPersistence(signUpRequest);
                const signUpRequestDocument = await this.signUpResquestSchema.create(pers);
                return SignUpRequestMap.toDomain(signUpRequestDocument);
            } else {
                return null;
            }
        } catch (e) {
            return Promise.reject(e);
        }
    }

}
