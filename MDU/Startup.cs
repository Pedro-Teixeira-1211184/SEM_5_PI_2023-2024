using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using MySql.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Shared;

using Microsoft.Extensions.Logging;
using System;

//What is beeing used, here:
//User Related
using DDDSample1.Infrastructure.SystemUser;
using DDDSample1.Domain.SystemUser;
//Task Related
using DDDSample1.Infrastructure.Tasks;
using DDDSample1.Domain.Tasks;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1
{
    public class Startup

    {
        private readonly ILogger<Startup> _logger;


        public Startup(IConfiguration configuration, ILogger<Startup> logger)
        {
            Configuration = configuration;
            _logger = logger;

        }

        public IConfiguration Configuration { get; }
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddDbContext<DDDSample1DbContext>(opt =>
                opt.UseMySQL("Server=vsgate-s1.dei.isep.ipp.pt;Port=10931;Database=Clients;user=root;Password=ihZ1b8BSFTQ1"));
            
            ConfigureMyServices(services);

            // Add logging
            services.AddCors(options =>
            {
                options.AddPolicy("AllowAllOrigins",
                    builder =>
                    {
                        builder.AllowAnyOrigin()
                               .AllowAnyMethod()
                               .AllowAnyHeader();
                    });
            });


            services.AddMvc(options => options.EnableEndpointRouting = false);


            services.AddControllers().AddNewtonsoftJson();
        }






        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }

            app.UseCors("AllowSpecificOrigin");
            app.UseCors("AllowOrigin");
            app.UseCors("AllowAllOrigins");
            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });

        }
        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<IUserRepository, UserRepository>();
            services.AddTransient<UserService>();

            services.AddTransient<ITaskRepository, TaskRepository>();
            services.AddTransient<TaskService>();
        }

    }
}
