import {Inject, Service} from 'typedi';
import config from '../../config';

//import MailerService from './mailer.ts.bak';
import IUserService from '../services/IServices/IUserService';
import {UserMap} from "../mappers/UserMap";
import {IUserDTO} from '../dto/IUserDTO';

import IUserRepo from './IRepos/IUserRepo';
import IRoleRepo from './IRepos/IRoleRepo';

import {User} from '../domain/user';
import {Result} from "../core/logic/Result";
import * as crypto from "crypto";

@Service()
export default class UserService implements IUserService {
    constructor(
        @Inject(config.repos.user.name) private userRepo: IUserRepo,
        @Inject(config.repos.role.name) private roleRepo: IRoleRepo
    ) {
    }


    public async SignUp(dto: IUserDTO): Promise<Result<IUserDTO>> {
        try {
            const userOrError = await User.create(dto);

            if (userOrError.isFailure) {
                return Result.fail<IUserDTO>(userOrError.errorValue());
            }

            //verify if role exists
            const roleExists = await this.getRole(dto.role);

            if (roleExists.isFailure) {
                console.log('Role does not exist');
                return Result.fail<IUserDTO>(roleExists.errorValue());
            }

            const user: User = userOrError.getValue();

            //verify if email already exists
            const emailAlreadyExists = await this.userRepo.findByEmail(user.email);

            if (emailAlreadyExists != null) {
                return Result.fail<IUserDTO>("Email already exists");
            }

            //hash password
            user.password = await this.hashPassword(user.password);

            await this.userRepo.save(user);

            const userDTO = UserMap.toDTO(user) as IUserDTO;

            return Result.ok<IUserDTO>(userDTO);
        } catch (e) {
            throw e;
        }
    }

    public async SignIn(email: string, password: string): Promise<Result<IUserDTO>> {
        //verify if email exists
        const user = await this.userRepo.findByEmail(email);
        const result = await this.verifyPassword(password, user.password)
        console.log(result);
        throw new Error("Method not implemented.");
    }

    private async getRole(roleName: string): Promise<Result<string>> {

        const role = await this.roleRepo.findByName(roleName);
        const found = !!role;

        if (found) {
            return Result.ok<string>(role.name);
        } else {
            return Result.fail<string>("Couldn't find role by name=" + roleName);
        }
    }

    private async hashPassword(password: string): Promise<string> {
        return crypto.createHmac('sha256', '10').update(password).digest('hex');
    }

    private async verifyPassword(password: string, hashedPassword: string): Promise<boolean> {
        const hashedAttempt = await this.hashPassword(password);
        return hashedAttempt === hashedPassword;
    }

}
