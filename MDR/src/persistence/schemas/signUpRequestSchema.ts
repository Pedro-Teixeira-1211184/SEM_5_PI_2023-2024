import mongoose from 'mongoose';
import {ISignUpRequestPersistence} from "../../dataschema/ISignUpRequestPersistence";
import {SignUpRequest} from "../../domain/signUpRequest";

const SignUpRequestSchema = new mongoose.Schema(
  {
    signUpRequestDomainId: {type: String, unique: true},
    signUpRequestFirstName: {type: String, required: true},
    signUpRequestLastName: {type: String, required: true},
    signUpRequestNif: {type: String, required: true},
    signUpRequestEmail: {type: String, lowercase: true, unique: true},
    signUpRequestPassword: {type: String, required: true}
  },
  {timestamps: true},
);

SignUpRequestSchema.index({signUpRequestDomainId: 1}, {unique: true});
export default mongoose.model<ISignUpRequestPersistence & mongoose.Document>('SignUpRequest', SignUpRequestSchema);
