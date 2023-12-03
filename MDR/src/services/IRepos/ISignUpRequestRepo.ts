import {Repo} from "../../core/infra/Repo";
import {User} from "../../domain/user";
import {UserEmail} from "../../domain/userEmail";
import {SignUpRequest} from "../../domain/signUpRequest";

export default interface ISignUpRequestRepo extends Repo<SignUpRequest> {
  save(signUpRequest: SignUpRequest): Promise<SignUpRequest>;
  findByEmail(email: UserEmail): Promise<SignUpRequest>;
  delete(signUpRequest: SignUpRequest): Promise<void>;
  exists(signUpRequest: SignUpRequest): Promise<boolean>;
}
