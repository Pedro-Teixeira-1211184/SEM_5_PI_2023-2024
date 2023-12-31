﻿// <auto-generated />
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Migrations;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace MDU.Migrations
{
    [DbContext(typeof(DDDSample1DbContext))]
    [Migration("20231231012106_RemoveUsername")]
    partial class RemoveUsername
    {
        protected override void BuildTargetModel(ModelBuilder modelBuilder)
        {
#pragma warning disable 612, 618
            modelBuilder
                .HasAnnotation("Relational:MaxIdentifierLength", 64)
                .HasAnnotation("ProductVersion", "5.0.5");

            modelBuilder.Entity("DDDSample1.Domain.SystemUser.User", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("varchar(767)");

                    b.Property<string>("Email")
                        .HasColumnType("varchar(767)");

                    b.Property<string>("FirstName")
                        .HasColumnType("text");

                    b.Property<string>("LastName")
                        .HasColumnType("text");

                    b.Property<string>("NIF")
                        .HasColumnType("varchar(767)");

                    b.Property<string>("Password")
                        .HasColumnType("text");

                    b.Property<string>("Role")
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.HasIndex("Email")
                        .IsUnique();

                    b.HasIndex("NIF")
                        .IsUnique();

                    b.ToTable("Users");
                });

            modelBuilder.Entity("DDDSample1.Domain.Tasks.Task", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("varchar(767)");

                    b.Property<string>("Description")
                        .HasColumnType("text");

                    b.Property<string>("EndFloorCode")
                        .HasColumnType("text");

                    b.Property<int>("EndX")
                        .HasColumnType("int");

                    b.Property<int>("EndY")
                        .HasColumnType("int");

                    b.Property<string>("RobotCode")
                        .HasColumnType("text");

                    b.Property<string>("StartFloorCode")
                        .HasColumnType("text");

                    b.Property<int>("StartX")
                        .HasColumnType("int");

                    b.Property<int>("StartY")
                        .HasColumnType("int");

                    b.Property<string>("TaskState")
                        .HasColumnType("text");

                    b.Property<string>("TaskType")
                        .HasColumnType("text");

                    b.Property<string>("UserEmail")
                        .HasColumnType("text");

                    b.Property<string>("UserID")
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Tasks");
                });
#pragma warning restore 612, 618
        }
    }
}
